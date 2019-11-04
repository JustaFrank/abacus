import { useState, ChangeEvent } from 'react'

interface FormConfig {
  [key: string]: string
}

export const useForm = (config: FormConfig) => {
  const [values, setValues] = useState(config)
  const props = Object.fromEntries(
    Object.entries(values).map(([key, value]) => [
      key,
      {
        value,
        onChange: (
          event:
            | ChangeEvent<HTMLInputElement>
            | ChangeEvent<HTMLTextAreaElement>
        ) => setValues({ ...values, [key]: event.target.value })
      }
    ])
  )
  return { props, values }
}

export const useFormInput = (initialValue = '') => {
  const [value, setValue] = useState(initialValue)
  const onChange = (
    event: ChangeEvent<HTMLInputElement> | ChangeEvent<HTMLTextAreaElement>
  ) => setValue(event.target.value)
  return { value, onChange }
}
