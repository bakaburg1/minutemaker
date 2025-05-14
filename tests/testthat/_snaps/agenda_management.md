# convert_agenda_times validates input times with specific warnings and error

    Code
      convert_agenda_times(invalid_from_item, convert_to = "seconds",
        event_start_time = "09:00")
    Condition
      Warning:
      All formats failed to parse. No formats found.
      Warning:
      Agenda element validation failed:
      i Element details: `list(session = NULL, title = "Test Talk", from = "invalid", to = "10:00")`
      x Agenda element field from time not interpretable: "invalid"
      Warning:
      All formats failed to parse. No formats found.
      Error in `if (diff < 0) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      convert_agenda_times(invalid_to_item, convert_to = "seconds", event_start_time = "09:00")
    Condition
      Warning:
      All formats failed to parse. No formats found.
      Warning:
      Agenda element validation failed:
      i Element details: `list(session = NULL, title = "Test Talk", from = "09:00", to = "invalid")`
      x Agenda element field to time not interpretable: "invalid"
      Warning:
      All formats failed to parse. No formats found.
      Error in `if (diff < 0) ...`:
      ! missing value where TRUE/FALSE needed

# convert_agenda_times handles mixed time formats with specific warnings and error

    Code
      convert_agenda_times(mixed_agenda)
    Condition
      Warning:
      Agenda element validation failed:
      x The agenda element from and to times are not of the same class:
      i from (<character>): "09:00"
      i to (<numeric>): 3600
      Warning:
      Agenda element validation failed:
      i Element details: `list(from = "09:00", to = 3600)`
      x Agenda element 'from' time should precede 'to' time:
        from: "09:00"
        to: 3600
      Warning:
      Agenda element validation failed:
      x The agenda element from and to times are not of the same class:
      i from (<numeric>): 3600
      i to (<character>): "11:00"
      Error in `convert_agenda_times()`:
      ! Agenda times must be all of the same class across the agenda.

