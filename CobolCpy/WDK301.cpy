000100 01  ETI-WDK301.                                                          
000200*                                 ARTIKELINFORMATION                      
000300*                                 FÖR ETIKETT UTSKRIFT                    
000400*                                 FYSISK NYCKEL IDARTNR                   
000500     03 ETI-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ETI-DAREGDAT         PIC 9(8).                                    
000900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001000*                                 REGISTRATION DATE (YYYYMMDD)            
001100     03 ETI-IDARTNR-ETIK     PIC S9(9)           COMP-3.                  
001200*                                 ARTNR FÖR LEVERANTÖRENS BEABS E         
001300*                                 TIKETTER                                
001400*                                 PART NUMBER FOR THE SUPPLIER BE         
001500*                                 ABS LABELS                              
001600     03 ETI-IDLAYOUT         PIC X(10).                                   
001700*                                 ETIKETTSLAYOUT ID                       
001800*                                 LABELLAYOUT ID                          
001900     03 ETI-IDSORTIM         PIC X(3).                                    
002000*                                 ID FÖR SORTIMENT                        
002100*                                 ID OF ASSORTMENT                        
002200     03 ETI-IDUSER           PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500     03 ETI-TEETIK-INT       PIC X(60).                                   
002600*                                 ETIKETT KOMMENTAR                       
002700*                                 LABEL REMARKS NOTE                      
002800     03 ETI-TIUPPDAT         PIC S9(7)           COMP-3.                  
002900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003000*                                 UPDATING DATE     (YYMMDD)              
003100*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
