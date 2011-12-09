import Data.IntervalMap as IM

type Person = String
type Details = String

-- For readability, represent timestamps as strings. Also, to keep it shorter,
-- we will only use time of the day in this example.
type Time = String
type TimeSpan = Interval Time

-- We have a time span include its start time but not its end time.
mkTimeSpan :: Time -> Time -> TimeSpan
mkTimeSpan from to = IntervalCO from to

type Appointments = IM.IntervalMap Time [(Person, Details)]

noAppointments :: Appointments
noAppointments = IM.empty

addAppointment :: Person -> Time -> Time -> Details -> Appointments -> Appointments
addAppointment who from to what apps = IM.insertWith (++) (mkTimeSpan from to) [(who, what)] apps

sampleApps :: Appointments
sampleApps = addAppointment "Paul" "09:00" "11:00" "Dentist" $
             addAppointment "John" "10:00" "11:30" "Meeting" $
             addAppointment "Rosy" "10:00" "11:30" "Shopping" $
             addAppointment "Lisa" "08:45" "09:15" "Bank" $
             noAppointments

appointmentsAt :: Time -> Appointments -> [(TimeSpan, Person, Details)]
appointmentsAt t apps = [ (ts, p, d) | (ts, ps) <- hits, (p,d) <- ps ]
  where
    hits :: [(TimeSpan, [(Person, Details)])]
    hits = IM.searchPoint t apps

appointmentsDuring :: Time -> Time -> Appointments -> [(TimeSpan, Person, Details)]
appointmentsDuring from to apps = [ (ts, p, d) | (ts, ps) <- hits, (p,d) <- ps ]
  where
    hits :: [(TimeSpan, [(Person, Details)])]
    hits = IM.searchInterval (mkTimeSpan from to) apps

main :: IO ()
main = do putStrLn (show (appointmentsAt "09:00" sampleApps))
          putStrLn (show (appointmentsDuring "09:30" "10:30" sampleApps))
	  putStrLn (show (IM.toAscList sampleApps))
