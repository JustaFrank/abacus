import React, { useState } from 'react'
import styled from 'styled-components'
import { RouteComponentProps, Link } from '@reach/router'

import { CalculatorField } from '../calculator/CalculatorField'
import { Page, PageHeading, PageDescription } from '../page/Page'
import { Button } from '../common/Button'
import { useUser } from '../../context/user-context'
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

const RegisterLink = styled(Link)`
  color: #f39c12;

  :hover {
    color: #dc8e11;
    text-decoration: underline;
  }
`

export const Login: React.FC<RouteComponentProps> = () => {
  const { login } = useUser()
  const { props, values } = useForm({ email: '', password: '' })
  const [isLoading, setIsLoading] = useState(false)

  const submitForm = async () => {
    setIsLoading(true)
    try {
      await login(values.email, values.password)
    } catch (err) {
      alert('Invalid email or password.')
    }
    setIsLoading(false)
  }

  return (
    <Page>
      <PageHeading>Login</PageHeading>
      <PageDescription>
        Welcome back! If you do not have an account, you can create one{' '}
        <RegisterLink to="/register">here</RegisterLink>.
      </PageDescription>
      <InputContainer>
        <Label>email</Label>
        <CalculatorField>
          <UnstyledInput {...props.email} type="text" />
        </CalculatorField>
      </InputContainer>
      <InputContainer>
        <Label>password</Label>
        <CalculatorField>
          <UnstyledInput {...props.password} type="password" />
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
        login
      </Button>
    </Page>
  )
}
