import React, { useState } from 'react'
import styled from 'styled-components'
import { RouteComponentProps } from '@reach/router'

import { CalculatorField } from '../calculator/CalculatorField'
import { Page, PageHeading, PageDescription } from '../page/Page'
import { Button } from '../common/Button'
import { useUser } from '../../context/user-context'
import { useEnterKeypress } from '../../hooks/use-enter-keypress'
import { useForm } from '../../hooks/use-form'

const UnstyledInput = styled.input`
  width: 100%;
  border: none;
  outline: none;
  font: inherit;
  padding: 0;
  height: inherit;
`

const Label = styled.div`
  font-size: 14px;
  font-weight: bolder;
  margin-bottom: 4px;
`

const InputContainer = styled.div`
  padding-bottom: 12px;
`

export const Register: React.FC<RouteComponentProps> = () => {
  const { register } = useUser()
  const { props, values } = useForm({ name: '', email: '', password: '' })
  const [isLoading, setIsLoading] = useState(false)

  const submitForm = async () => {
    setIsLoading(true)
    try {
      await register(values.name, values.email, values.password)
    } catch (err) {
      alert('Failed to create account.')
    }
    setIsLoading(false)
  }

  const handleKeypress = useEnterKeypress(submitForm)

  return (
    <Page>
      <PageHeading>Register</PageHeading>
      <PageDescription>
        Please fill out the following form to create an account. (This app isn't
        really in production, so feel free to use a fake email)
      </PageDescription>
      <InputContainer>
        <Label>name</Label>
        <CalculatorField>
          <UnstyledInput
            {...props.name}
            type="text"
            onKeyPress={handleKeypress}
          />
        </CalculatorField>
      </InputContainer>
      <InputContainer>
        <Label>email</Label>
        <CalculatorField>
          <UnstyledInput
            {...props.email}
            type="text"
            onKeyPress={handleKeypress}
          />
        </CalculatorField>
      </InputContainer>
      <InputContainer>
        <Label>password (6+ characters)</Label>
        <CalculatorField>
          <UnstyledInput
            {...props.password}
            type="password"
            onKeyPress={handleKeypress}
          />
        </CalculatorField>
      </InputContainer>
      <Button
        width="160px"
        backgroundColor="orange"
        color="black"
        style={{ marginTop: '12px' }}
        onClick={submitForm}
        loading={isLoading}
      >
        register
      </Button>
    </Page>
  )
}
