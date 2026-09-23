000010 01  MOD-W0O60101.                                                        
000020*                                 COPYTEXT FÖR MOD W0O60101               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-IDLTERM-IN       PIC X(2).                                    
000080*                                 MFS BEHANDLING AV INPUTFÄLT             
000090     03 MOD-IDLTERM-UT       PIC X(8).                                    
000100*                                 LOGISKT TERMINALNAMN                    
000110     03 MOD-IDLIST-IN        PIC X(2).                                    
000120*                                 MFS BEHANDLING AV INPUTFÄLT             
000130     03 MOD-IDLIST-UT        PIC X(10).                                   
000140*                                 LISTIDENTITET                           
000150     03 MOD-TIREGDAT-IN      PIC X(2).                                    
000160*                                 MFS BEHANDLING AV INPUTFÄLT             
000170     03 MOD-TIREGDAT-UT      PIC 9(6).                                    
000180*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000190     03 MOD-IDLTERM-BACKUP-ATTR                                           
000200                             PIC X(2).                                    
000210*                                 MFS ATTRIBUTFÄLT                        
000220     03 MOD-IDLTERM-BACKUP   PIC X(8).                                    
000230*                                 LOGISKT TERMINALNAMN                    
000240     03 MOD-TIKLOCK-9KOMPL-ENTER                                          
000250                             PIC 9(9).                                    
000260*                                 TID LAGRAT SOM 9-KOMPLEMENT             
000270*                                                                         
000280     03 MOD-TIKLOCK-9KOMPL-NEXT                                           
000290                             PIC 9(9).                                    
000300*                                 TID LAGRAT SOM 9-KOMPLEMENT             
000310*                                                                         
000320     03 MOD-RAD              OCCURS 12 TIMES.                             
000330        05 MOD-KDSVAR-ATTR   PIC X(2).                                    
000340*                                 MFS ATTRIBUTFÄLT                        
000350        05 MOD-KDSVAR        PIC X.                                       
000360*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000370        05 MOD-TIKLOCK       PIC 9(9).                                    
000380*                                 KLOCKSLAG (TTMMSSTH)                    
000390        05 MOD-IDLIST        PIC X(10).                                   
000400*                                 LISTIDENTITET                           
000410        05 MOD-BEPRTLST      PIC X(25).                                   
000420*                                 LOGISK LISTA+PRINTER BENÄMNING          
000430        05 MOD-KVANTEX-PRINTAD                                            
000440                             PIC 9.                                       
000450*                                 ANTAL GÅNGER LISTAN ÄR                  
000460*                                 UTSKRIVEN                               
000470        05 MOD-FLSKRIV       PIC X.                                       
000480*                                 JA = ÅTERSTART AV                       
000490*                                 BEGÄRD LISTA                            
000500     03 MOD-TEMFSINF         PIC X(61).                                   
000510*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W0O60101    LENGTH=751                                       
