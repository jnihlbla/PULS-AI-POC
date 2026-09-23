000100 01  W23361.                                                              
000200*                                 VECKOTRANSAR TILL CARPAC                
000300*                                 INSAMLAT DATA                           
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 REKSIFFR             PIC 9.                                       
000700*                                 KONTROLLSIFFRA                          
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 IDANSK               PIC S9(3)           COMP-3.                  
001100*                                 ANSKAFFARNUMMER                         
001200     03 KDVVKL               PIC S9              COMP-3.                  
001300*                                 VOLYMVÄRDESKLASS                        
001400     03 KDAGE                PIC X.                                       
001500*                                 AGE-CODE                                
001600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001700*                                 ARTIKELSTANDARDPRIS                     
001800     03 KDERS                PIC S9(3)           COMP-3.                  
001900*                                 ERSÄTTNINGSKOD                          
002000     03 KVUTJFEL             PIC S9(6)V9(1)      COMP-3.                  
002100*                                 UTJÄMNAT FEL                            
002200     03 KVMAD-TOT            PIC S9(6)V9(1)      COMP-3.                  
002300*                                 TOTALT PROGNOSFEL                       
002400     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
002500*                                 TOTALT PERIODBEHOV                      
002600     03 KVLS                 PIC S9(7)           COMP-3.                  
002700*                                 LAGERSALDO                              
002800     03 KVROS                PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERSALDO                          
003000     03 TEARTNOT-1           PIC X(40).                                   
003100*                                 ARTIKEL NOTERING                        
003200     03 TEARTNOT-2           PIC X(40).                                   
003300*                                 ARTIKEL NOTERING                        
003400     03 TIAVIDAT-SEN         PIC S9(7)           COMP-3.                  
003500*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
003600     03 KVBR                 PIC S9(7)           COMP-3.                  
003700*                                 BESTÄLLNINGSREST                        
003800     03 PRARTBTO-MARK        PIC S9(7)V9(2)      COMP-3.                  
003900*                                 BRUTTOPRIS PER MARKNAD (FOB)            
004000     03 KVAVROP              PIC S9(7)           COMP-3.                  
004100*                                 AVROPSKVANTITET                         
004200*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
