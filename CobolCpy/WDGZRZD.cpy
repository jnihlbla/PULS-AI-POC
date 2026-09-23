000010 01  RZD-WDGZRZD.                                                         
000020*                                 POSTTYP RZD                             
000030*                                 TILL ORDERINGÅNGSSTATISTIK              
000040*                                 VID AVBOKNING I DAGORDER                
000050     03 RZD-IDPTYP           PIC X(3).                                    
000060*                                 POSTTYP                                 
000070     03 RZD-IDARTNR          PIC S9(9)           COMP-3.                  
000080*                                 ARTIKELNUMMER                           
000090     03 RZD-KDBEHX           PIC X.                                       
000100*                                 BEHANDLINGSKOD-X                        
000110     03 RZD-KDOI             PIC S9(3)           COMP-3.                  
000120*                                 ORDERINGÅNGSTYP                         
000130     03 RZD-KVOI             PIC S9(7)           COMP-3.                  
000140*                                 ORDERINGÅNG I STYCK PER TIDSENH         
000150     03 RZD-KVOT             PIC S9(7)           COMP-3.                  
000160*                                 ANTAL ORDERTRÄFF                        
000170     03 RZD-TIAAP-AVBOK      PIC S9(3)           COMP-3.                  
000180*                                 AVBOKNINGSPERIOD (ÅÅP)                  
000190     03 RZD-IDDC             PIC X(2).                                    
000200*                                 IDENTIFIERARE LAGER                     
000210     03 RZD-IDDC-DAY         PIC X(2).                                    
000220*                                 IDENTIFIERARE DAGORDERLAGER             
