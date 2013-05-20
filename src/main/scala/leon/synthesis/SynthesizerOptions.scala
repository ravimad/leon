package leon
package synthesis

case class SynthesizerOptions(
  generateDerivationTrees: Boolean    = false,
  filterFuns: Option[Set[String]]     = None,
  searchWorkers: Int                  = 1,
  firstOnly: Boolean                  = false,
  timeoutMs: Option[Long]             = None,
  costModel: CostModel                = CostModel.default,

  // Cegis related options
  cegisGenerateFunCalls: Boolean      = false,
  cegisUseCETests: Boolean            = true,
  cegisUseCEPruning: Boolean          = true,
  cegisUseBPaths: Boolean             = true
)