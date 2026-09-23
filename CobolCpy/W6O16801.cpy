000100 01  MOD-W6O16801.                                                        
000200*                                 MOD COPYTEXT FÖR W6O16600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-KVANTAL-ETIK-ATTR                                             
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-KVANTAL-ETIK-IN  PIC Z(5)9.                                   
001500*                                 ANTAL BESTÄLLDA ETIKETTER TILL          
001600*                                 UTSKRIFT                                
001700     03 MOD-IDPRTLST-ATTR    PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDPRTLST-IN      PIC X(8).                                    
002000*                                 LOGISK PRINTER+LISTA IDENTITET          
002100     03 MOD-BEARTURS-JUST-ATTR                                            
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-BEARTURS-JUST-IN PIC X(15).                                   
002500*                                 ARTIKELURSPRUNGSLAND BENÄMNING          
002600     03 MOD-BEARTURS         PIC X(15).                                   
002700*                                 ARTIKELURSPRUNGSLAND BENÄMNING          
002800     03 MOD-KVQPACK-JUST-ATTR                                             
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-KVQPACK-JUST-IN  PIC Z(4)9.                                   
003200*                                 JUSTERAD KVANT                          
003300     03 MOD-KVQPACK-0        PIC Z(4)9.                                   
003400*                                 ANTAL I Q0 FÖRPACKNING                  
003500     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
003600*                                 ANTAL I Q1 FÖRPACKNING                  
003700     03 MOD-KVQPACK-2        PIC Z(4)9.                                   
003800*                                 ANTAL I Q2 FÖRPACKNING                  
003900     03 MOD-KVQPACK-JUST     PIC Z(4)9.                                   
004000*                                 JUSTERAD KVANT                          
004100     03 MOD-KDSORT           PIC X(2).                                    
004200*                                 SORT-KOD                                
004300     03 MOD-IDBATCH-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDBATCH-IN       PIC X(18).                                   
004600*                                 ID FÖR LEVERENÖRS BATCH UPPBYGD         
004700*                                 ENLIGT FÖLJANDE:                        
004800*                                  SIFFRA 1-5       LEV.NR                
004900*                                  SIFFRA 6           0                   
005000*                                  SIFFRA 7-18      LEV. PARTINR.         
005100*                                                                         
005200     03 MOD-TEETIK-INT       PIC X(60).                                   
005300*                                 ETIKETT KOMMENTAR                       
005400     03 MOD-BEART            PIC X(25).                                   
005500*                                 ARTIKELBENÄMNING                        
005600     03 MOD-IDARTNR-ETIK     PIC Z(8)9.                                   
005700*                                 ARTNR FÖR LEVERANTÖRENS BEABS E         
005800*                                 TIKETTER                                
005900     03 MOD-IDUSER           PIC X(8).                                    
006000*                                 ANVÄNDARENS SÄKERHETS ID                
006100     03 MOD-TIUPPDAT         PIC 9(6).                                    
006200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006300     03 MOD-IDLAYOUT         PIC X(10).                                   
006400*                                 ETIKETTSLAYOUT ID                       
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 334 BYTES                                 
