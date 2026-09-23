000100 01  MOD-W4O51101.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O51101                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-FLSORT           PIC X.                                       
002100*                                 SORTERINGSFLAGGA (J/N)                  
002200     03 MOD-FLSORT-UT        PIC X.                                       
002300*                                 SORTERINGSFLAGGA (J/N)                  
002400     03 MOD-FLSOFT           PIC X.                                       
002500*                                 FLAGGA SOFTVARA                         
002600     03 MOD-FLSOFT-UT        PIC X.                                       
002700*                                 FLAGGA SOFTVARA                         
002800     03 MOD-FLPROF           PIC X.                                       
002900*                                 PROFORMA-MÄRKNING                       
003000     03 MOD-FLPROF-UT        PIC X.                                       
003100*                                 PROFORMA-MÄRKNING                       
003200     03 MOD-TEDDI            PIC X(11).                                   
003300*                                 TEXTFÄLT DDI                            
003400     03 MOD-LINES-GRP        OCCURS 14 TIMES.                             
003500*                                 RADER (SVENSKA)                         
003600        05 MOD-IDTRANS-HOPP  PIC X(4).                                    
003700*                                 BILDNUMMER                              
003800        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003900*                                 KUNDNUMMER                              
004000        05 MOD-IDORDNR7      PIC Z(6)9.                                   
004100*                                 ORDERNUMMER                             
004200        05 MOD-IDPRODNR      PIC Z(6)9.                                   
004300*                                 PRODUKTIONSNUMMER                       
004400        05 MOD-IDDC          PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600        05 MOD-KDFRAKT       PIC Z9.                                      
004700*                                 FRAKTSÄTT DC TILL KUND                  
004800        05 MOD-KDORDKL       PIC 9.                                       
004900*                                 ORDERKLASS                              
005000        05 MOD-KDORDSTA      PIC X(2).                                    
005100*                                 VOLVOORDERSTATUS                        
005200        05 MOD-BEKUNDRF      PIC X(10).                                   
005300        05 MOD-TIREGDAT      PIC 9(6).                                    
005400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005500        05 MOD-IDUSER        PIC X(8).                                    
005600*                                 ANVÄNDARENS SÄKERHETS ID                
005700        05 MOD-SUORDV        PIC Z(7)9.9(2).                              
005800*                                 SUMMA ORDERVÄRDE                        
005900        05 MOD-TEASTRIX      PIC X.                                       
006000*                                 ASTERISK                                
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 1078 BYTES                                
