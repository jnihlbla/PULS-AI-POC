000100 01  MOD-W4O35301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD  4353             
000300*                                 MANUELLT UTTAG AV PLOCKSATS             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRC-IN         PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MOD-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MOD-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MOD-IDPLKLST-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDPLKLST-UT      PIC X(3).                                    
001900*                                 PLOCKLISTNUMMER                         
002000     03 MOD-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-IDTRP-IN         PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDTRP-UT.                                                     
002700*                                 TRANSPORTIDENTITET                      
002800        05 MOD-IDTRPLOS      PIC X(3).                                    
002900*                                 TRANSPORTLÖSNING                        
003000        05 MOD-IDTRPVAR      PIC X(2).                                    
003100*                                 TRANSPORTLÖSNINGSGRUPP                  
003200     03 MOD-TIAAMMDD-IN      PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-TIAAMMDD-UT      PIC X(6).                                    
003500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003600     03 MOD-TIHHMM-IN        PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-TIHHMM-UT        PIC X(4).                                    
003900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004000     03 MOD-IDDISTR-IN       PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-IDDISTR-UT       PIC X(4).                                    
004300*                                 DISTRIKTNUMMER                          
004400     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
004700*                                 KUNDNUMMER                              
004800     03 MOD-IDORDNR7-IN      PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 MOD-IDORDNR7-UT      PIC X(7).                                    
005100*                                 ORDERNUMMER                             
005200     03 MOD-KDPRT-PU-ATTR    PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDPRT-PU         PIC X(3).                                    
005500*                                 PRINTERKOD PACKUNDERLAG                 
005600     03 MOD-KDPRT-PLE-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KDPRT-PLE        PIC X(3).                                    
005900*                                 PRINTERKOD PLOCKETIKETTER               
006000     03 MOD-MIXAT-ATTR       PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-MIXAT            PIC X.                                       
006300*                                 ALLMÄN SVARSFLAGGA                      
006400     03 MOD-TEMFSINF         PIC X(55).                                   
006500*                                 INFORMATIONSMEDDELANDE                  
