package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 0)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
    extends BrPredictor(fetch_width, history_length)(p)
{
  require (coreInstBytes == 4)
  require (fetch_width == 1)

  // Table of 10-bit counters (init to 0) for Local History
  val localHT = Reg(
    init = Vec(Seq.fill(1024) {UInt("b00", width = 4)})
  )

  // Table of 3-bit counters (init to 0) for Local Prediction
  val localPred = Reg(
    init = Vec(Seq.fill(16) {UInt("b00", width = 3)})
  )

  // Table of 2-bit counters (init to 0) for Global Prediction
  val globalPred = Reg(
    init = Vec(Seq.fill(math.pow(2, history_length).toInt) {UInt("b00", width = 2)})
  )

  // Table of 2-bit counters (init to 0) for Choice Prediction
  val choicePred = Reg(
    init = Vec(Seq.fill(math.pow(2, history_length).toInt) {UInt("b00", width = 2)})
  )

  // Creating global path history register
  val globalPH = Reg(
    init = UInt("b00", width = history_length)
  )

  // pause prediction
  val stall = !io.resp.ready

  // Local prediction
  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(coreInstBytes))
  val s1_local_hist = localHT(s1_r_idx)
  val s1_local_pred = RegEnable(localPred(s1_local_hist), !stall)

  // Global Prediction
  val s1_global_pred = RegEnable(globalPred(globalPH), !stall)

  // Choice Prediction
  io.resp.valid := !this.disable_bpd
  val s1_choice_pred = choicePred(globalPH)
  io.resp.bits.takens := Mux(s1_choice_pred(1), s1_global_pred(1), s1_local_pred(2))
  val info_vec = Vec.fill(3) {UInt("b00", width = 4)}
  io.resp.bits.info := RegNext(info_vec.asUInt())

  // On commit, check to see if branch was actually taken and update state
  val s1_commit_en = this.commit.valid
  val commit_vec = Vec(3, UInt(4.W)).fromBits(this.commit.bits.info.info)
  val s1_commit_idx = Wire(commit_vec(0))
  val s1_commit_lh = Wire(commit_vec(1))
  val s1_commit_gh = Wire(commit_vec(2))
  val s1_commit_taken = this.commit.bits.ctrl.taken(0)

  // Index into tables to get previous states
  val s2_commit_idx = RegEnable(s1_commit_idx, s1_commit_en)
  val s2_commit_lh = RegEnable(s1_commit_lh, s1_commit_en)
  val s2_commit_gh = RegEnable(s1_commit_gh, s1_commit_en)
  val s2_commit_local_pred = RegEnable(localPred(s1_commit_lh), s1_commit_en)
  val s2_commit_global_pred = RegEnable(globalPred(s1_commit_gh), s1_commit_en)
  val s2_commit_choice_pred = RegEnable(choicePred(s1_commit_gh), s1_commit_en)
  val s2_commit_taken = RegEnable(s1_commit_taken, s1_commit_en)
  val s2_commit_en = RegNext(s1_commit_taken)

  // Calculate Updated Values
  // Local
  val s2_commit_update_lp = Mux(s2_commit_taken,
    Mux(s2_commit_local_pred === "b111".U, s2_commit_local_pred, s2_commit_local_pred + 1.U),
    Mux(s2_commit_local_pred === "b000".U, s2_commit_local_pred, s2_commit_local_pred - 1.U))
  // Global
  val s2_commit_update_gp = Mux(s2_commit_taken,
    Mux(s2_commit_global_pred === "b11".U, s2_commit_global_pred, s2_commit_global_pred + 1.U),
    Mux(s2_commit_global_pred === "b00".U, s2_commit_global_pred, s2_commit_global_pred - 1.U))
  // Choice
  val s2_commit_pred_eq = s2_commit_global_pred === s2_commit_local_pred

  val s2_commit_update_cp = Mux(s2_commit_global_pred === s2_commit_local_pred,
    s2_commit_choice_pred,
    Mux(s2_commit_global_pred === s2_commit_taken,
      Mux(s2_commit_choice_pred === "b11".U, s2_commit_choice_pred, s2_commit_choice_pred + 1.U),
      Mux(s2_commit_choice_pred === "b00".U, s2_commit_choice_pred, s2_commit_choice_pred - 1.U)))

  // write back to table
  when (s2_commit_en) {
    localPred(s2_commit_lh) := s2_commit_update_lp
    globalPred(s2_commit_gh) := s2_commit_update_gp
    choicePred(s2_commit_gh) := s2_commit_update_cp

    globalPH := (globalPH << 1) + s2_commit_taken
    localHT(s2_commit_idx) := (s2_commit_lh << 1) + s2_commit_taken
  }
}