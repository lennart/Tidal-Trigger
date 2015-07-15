module Sound.Tidal.InputShape.QuNeo where


data Axis =
  Pressure {} |
  Velocity {} |
  Note {} |
  ControlChange {} |
  Direction {} |
  Location {} |
  PolyAftertouch {} |
  Width {} |
  X {} |
  Y {} |
  Z {}
  deriving (Show)

data Behavior =
  Toggle |
  Step { x :: Int} |
  Trigger |
  Stream
  deriving (Show)


type ControlBehaviour = (Behavior, Axis)

-- data Control =
--   Toggle { cid :: String, axes :: [Axis], } |

-- data ControlGroup =
--   Buttons


-- data Command =
--   Setter |
--   Getter |
--   Trigger |


data Group = Group { behaviours :: [ControlBehaviour], count :: Int } deriving (Show)

data InputShape = InputShape { controls :: [Group], toPreset :: (InputShape -> String) }

quNeo = InputShape {
  controls = [
     -- run x => will emit a list from 0 to x - 1. when composed will create a 2d-list (or matrix, if you will)
     Group [(Trigger, Note), (Stream, Pressure)] 4, -- <$> Group [(Trigger, Note), (Pressure, Stream)] 4 , -- in this case this will create a 4x4 matrix of controls, each uniquely identifying one pad on the QuNeo
     Group [(Stream, Location)] 2, -- will create two rotary controls in a list
     Group [(Step 4, ControlChange)] 2,-- step x y => will group x controls and have them step through values y values
     Group [(Step 4, ControlChange)] 2, -- in this case 2 controls are group and will pressing one will step its logical value by one in a given direction with a limit of 4
     Group [(Step 4, ControlChange)] 1, -- in this case it's just one control that steps one in the given direction with a limit of 4
     Group [(Toggle, ControlChange)] 1, -- will change the value from 0 to 1 or 1 to 0
     Group [(Toggle, ControlChange)] 1,
     Group [(Toggle, ControlChange)] 1,
     Group [(Stream, Location)] 4, -- sliders are two lists, each grouping four controls
     Group [(Stream, Location)] 4
  ],

  toPreset = (\x -> show $ controls x) -- converts InputShape into controller preset file importable in QuNeo editor
 }

-- attach sliders buttons -- attach a x => combines two control-groups into one by letting x control the logical values of a, in this case buttons (which are steppers through 4 values) will be able to `switch` the slider _behaviours_ to __one__ of __4__ banks.
