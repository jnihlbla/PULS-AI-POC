000100 01  W23671.                                                              
000200*                                                                         
000300*                                 INFO FRÅN                               
000400*                                 FÖRSENADE AVROP                         
000500*                                 OCH RO                                  
000600*                                                                         
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDLEVNR              PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 IDANSK               PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300     03 IDPLANGR-LEV         PIC S9              COMP-3.                  
001400*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
001500     03 KDERS                PIC S9(3)           COMP-3.                  
001600*                                 ERSÄTTNINGSKOD                          
001700     03 KVLS                 PIC S9(7)           COMP-3.                  
001800*                                 LAGERSALDO                              
001900     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
002000*                                 TOTALT PERIODBEHOV                      
002100     03 KVRESS               PIC S9(7)           COMP-3.                  
002200*                                 RESERVERAT ANTAL ARTIKLAR               
002300     03 KVROS                PIC S9(7)           COMP-3.                  
002400*                                 RESTORDERSALDO                          
002500     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002600*                                 DEL AV AK PÅ VÄG                        
002700     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
002800*                                 DEL AV AK SOM LIGGER I CDC              
002900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 ARTIKELSTANDARDPRIS                     
003100     03 KVAVIS-SEN           PIC S9(7)           COMP-3.                  
003200*                                 SENAST AVISERAT ANTAL                   
003300     03 TIAVIDAT-SEN         PIC S9(7)           COMP-3.                  
003400*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
003500     03 IDAVINR-SEN          PIC S9(7)           COMP-3.                  
003600*                                 AVINUMMER SENASTE INLEVERANS            
003700     03 FLPASS               PIC X.                                       
003800*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
