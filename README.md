# Introdução

O No show é um termo amplamente utilizado na literatura para descrever a falta de indivíduos em eventos agendados. Neste trabalho, analisaremos o \textit{no-show} dos pacientes em consultas médicas. 

O absenteísmo gera um grande impacto negativo nos estabelecimentos de saúde uma vez que gera um tempo ocioso para o profissional de saúde e equipamentos, gerando a baixa rentabilidade e a queda da qualidade do atendimento médico. 

Em geral, a solução adotada é a prática de \textit{overbooking}, em que mais pacientes do que se pode atender são agendados. Entretanto essa prática também apresenta diversos pontos negativos, como o aumento do tempo para o agendamento de novas consultas e, quando muitos pacientes comparecem a consulta, há um aumento do tempo de espera no estabelecimento e dificuldades do atendimento de qualidade pelos profissionais da saúde decorrente do excesso de pacientes.

Deste modo, com a facilidade de obtenção de dados sobre o paciente, uma nova e possível abordagem é a utilização de métodos estatísticos de aprendizagem para predizer a probabilidade do paciente comparecer a consulta médica e, desta forma, definir uma melhor estratégia para otimizar os agendamentos.  

## Objetivo

O objetivo deste trabalho é aplicar as técnicas de aprendizagem estatística para prever se um paciente irá faltar uma consulta médica dado um conjunto de fatores que podem influenciar na decisão de ir ao consultório. Assim, com esse problema de classificação supervisionada poderemos predizer quais grupos de pacientes são mais propensos a comparecer ou não às consultas médicas agendadas.

## Descrição dos dados

Os dados são compostos por 110.527 agendamentos de consulta no total de 62.299 pacientes em diversas faixas de idade, sendo 64\% de sexo feminino e 36\% de sexo masculino. O período da coleta dos dados foi entre novembro de 2015 e maio de 2016.

São coletados 14 variáveis:
\begin{itemize}
    \item \textit{PatienId}: Identificação de paciente;
    \item \textit{AppointmentID}: Identificação de cada agendamento;
    \item \textit{Gender}: Masculino(M) ou feminino(F);
    \item \textit{ScheduledDay}: A data da marcação de consulta, o dia agendado para visitar o médico;
    \item \textit{AppointmentDay}: A data de agendamento, o dia de registro do agendamento;
    \item \textit{Age}: Idade de paciente em anos;
    \item \textit{Neighbourhood}: Endereço da consulta;
    \item \textit{Scholarship}: Receber bolsa de família(1) ou não(0);
    \item \textit{Hipertension}: Ter hipertensão(1) ou não(0);
    \item \textit{Diabetes}: Ter diabetes(1) ou não(0);
    \item \textit{Alcoholism}: Beber bebida alcoólica(1) ou não(0);
    \item \textit{Handicap}: Paciente com deficiência(1) ou não(0);
    \item \textit{SMS\_received}: Paciente aceita a receber mensagens de aviso(1) ou não aceita(0);
    \item \textit{No\-show}: Paciente não compareceu na consulta(Yes) ou compareceu na consulta(No).
    
\end{itemize}

Os dados a serem utilizados foram obtidos no site do Kaggle: \textit{Medical Appointment No Shows} (https://www.kaggle.com/joniarroba/noshowappointments).

