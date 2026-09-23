000010*** EDIT ALLOWED                                                          
000010*                            *************************************        
000020*                            *** STANDARDKURS FÖR LEVA2.                  
000021*                            ***                                          
000022*                            *** ANVÄNDS FÖR BERÄKNING AV                 
000023*                            *** KURSDIFF FÖR KONTROLLRAPPPORT            
000024*                            *** TILL LEVA2.                              
000050*                            ***                                          
000051*                            *** LEVA2 OCH STANDARKURS EJ SAMMA.          
000052*                            ***                                          
000060*                            *************************************        
000070*                                                                         
000080 01  PRKURS-LEVA.                                                         
000081     03 FILLER.                                                           
000090        05 FILLER          PIC X(03)             VALUE 'NOK'.             
000091        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.92.              
000092     03 FILLER.                                                           
000093        05 FILLER          PIC X(03)             VALUE 'DKK'.             
000094        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.94.              
000096     03 FILLER.                                                           
000097        05 FILLER          PIC X(03)             VALUE 'FIM'.             
000098        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 1.32.              
000100     03 FILLER.                                                           
000101        05 FILLER          PIC X(03)             VALUE 'DEM'.             
000102        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 3.60.              
000110     03 FILLER.                                                           
000111        05 FILLER          PIC X(03)             VALUE 'NLG'.             
000112        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 3.20.              
000114     03 FILLER.                                                           
000115        05 FILLER          PIC X(03)             VALUE 'BEF'.             
000116        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.175.             
000122     03 FILLER.                                                           
000123        05 FILLER          PIC X(03)             VALUE 'GBP'.             
000124        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 10.50.             
000126     03 FILLER.                                                           
000127        05 FILLER          PIC X(03)             VALUE 'FRF'.             
000128        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 1.07.              
000130     03 FILLER.                                                           
000131        05 FILLER          PIC X(03)             VALUE 'CHF'.             
000132        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 3.90.              
000134     03 FILLER.                                                           
000135        05 FILLER          PIC X(03)             VALUE 'ITL'.             
000136        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.0047.            
000138     03 FILLER.                                                           
000139        05 FILLER          PIC X(03)             VALUE 'CAD'.             
000140        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 4.95.              
000142     03 FILLER.                                                           
000143        05 FILLER          PIC X(03)             VALUE 'USD'.             
000144        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 5.90.              
000146     03 FILLER.                                                           
000147        05 FILLER          PIC X(03)             VALUE 'MYR'.             
000149        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 2.22.              
000150     03 FILLER.                                                           
000151        05 FILLER          PIC X(03)             VALUE 'THB'.             
000153        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.21.              
000154     03 FILLER.                                                           
000155        05 FILLER          PIC X(03)             VALUE 'AUD'.             
000157        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 4.65.              
000158     03 FILLER.                                                           
000159        05 FILLER          PIC X(03)             VALUE 'HKD'.             
000160        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.70.              
000161     03 FILLER.                                                           
000163        05 FILLER          PIC X(03)             VALUE 'ATS'.             
000164        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.51.              
000166     03 FILLER.                                                           
000167        05 FILLER          PIC X(03)             VALUE 'ESP'.             
000168        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.056.             
000170     03 FILLER.                                                           
000171        05 FILLER          PIC X(03)             VALUE 'PTE'.             
000172        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.042.             
000174     03 FILLER.                                                           
000175        05 FILLER          PIC X(03)             VALUE 'SGD'.             
000177        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 3.70.              
000178     03 FILLER.                                                           
000179        05 FILLER          PIC X(03)             VALUE 'JPY'.             
000180        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 0.047.             
000182     03 FILLER.                                                           
000183        05 FILLER          PIC X(03)             VALUE 'IEP'.             
000184        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 9.50.              
000190     03 FILLER.                                                           
000191        05 FILLER          PIC X(03)             VALUE 'XEU'.             
000193        05 FILLER          PIC S9(6)V9(5) COMP-3 VALUE 7.40.              
000194 01  FILLER REDEFINES PRKURS-LEVA.                                        
000195     03 FILLER  OCCURS 23 TIMES.                                          
000196        05 LEVA-KDVALISO   PIC X(03).                                     
000200        05 LEVA-PRKURS     PIC S9(6)V9(5) COMP-3.                         
