library(tidyverse)
library(ggplot2)
library(easystats)
library(broom)

dat = read.csv('FacultSalaries_1995.csv')

colnames(dat)

dat2 = dat %>%
  pivot_longer(cols = c('AvgFullProfSalary', 'AvgAssocProfSalary', 'AvgAssistProfSalary'),
               names_to = 'Rank',
               values_to = 'Salary') %>%
  mutate(Rank = case_when(Rank == 'AvgFullProfSalary'~'Full',
                          Rank == 'AvgAssocProfSalary'~'Assoc',
                          Rank == 'AvgAssistProfSalary'~'Assist'))


dat2 %>%
  filter(Tier != 'VIIB') %>%
ggplot(aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  facet_wrap(~Tier) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



moda = aov(Salary ~ State + Tier + Rank, data = dat2)
summary(moda)




jun = read.csv('Juniper_Oils.csv')

which(colnames(jun) == 'alpha.pinene')
which(colnames(jun) == 'thujopsenal')

jun2 = jun %>%
  pivot_longer(cols = 11:33,
               names_to = "ChemicalID",
               values_to = 'Concentration') %>%
  mutate(ChemicalID = case_when(ChemicalID =='alpha.pinene' ~ 'alpha-pinene',
                                ChemicalID =='para.cymene' ~ 'para-cymene',
                                ChemicalID =='alpha.terpineol' ~ 'alpha-terpineol',
                                ChemicalID =='cedr.9.ene' ~ 'cedr-9-ene',
                                ChemicalID =='alpha.cedrene' ~ 'alpha-cedrene',
                                ChemicalID =='beta.cedrene' ~ 'alpha-cedrene',
                                ChemicalID =='beta.cedrene' ~ 'beta-cedrene',
                                ChemicalID =='cis.thujopsene' ~ 'cis-thujopsene',
                                ChemicalID =='alpha.himachalene' ~ 'alpha-himachalene',
                                ChemicalID =='beta.chamigrene' ~ 'beta-chamigrene',
                                ChemicalID =='compound.1' ~ 'compound 1',
                                ChemicalID =='alpha.chamigrene' ~ 'alpha-chamigrene',
                                ChemicalID =='beta.acorenol' ~ 'beta-acorenol',
                                ChemicalID =='alpha.acorenol' ~ 'alpha-acorenol',
                                ChemicalID =='gamma.eudesmol' ~ 'gamma-audesmol',
                                ChemicalID =='beta.eudesmol' ~ 'beta-eudesmol',
                                ChemicalID =='alpha.eudesmol' ~ 'alpha-eudesmol',
                                ChemicalID =='cedr.8.en.13.ol' ~ 'cedr-8-en-13-ol',
                                ChemicalID =='cedr.8.en.15.ol' ~ 'cedr-8-en-15-ol',
                                ChemicalID =='compound.2' ~ 'compound 2',
                                ChemicalID =='cuparene' ~ 'cuparene',
                                ChemicalID =='widdrol' ~ 'widdrol',
                                ChemicalID =='cedrol' ~ 'cedrol',
                                ChemicalID =='thujopsenal' ~ 'thujopsenal',))



jun2 %>%
  ggplot(aes(x = YearsSinceBurn, y = Concentration)) +
  geom_smooth() +
  facet_wrap(~ChemicalID, scales = 'free_y') +
  theme_minimal()

modb = glm(Concentration ~ YearsSinceBurn + ChemicalID, data = jun2)
summary(modb)

significant_results <- tidy(modb) %>%
  filter(p.value < 0.05)

print(significant_results)
