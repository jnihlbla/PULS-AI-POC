000100 01  W23673.                                                              
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
002900     03 KVOKS-VOR            PIC S9(7)           COMP-3.                  
003000*                                 ORDERKÖSALDO, VOR                       
003100     03 KVAVIS-SEN           PIC S9(7)           COMP-3.                  
003200*                                 SENAST AVISERAT ANTAL                   
003300     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 ARTIKELSTANDARDPRIS                     
003500     03 TIAVIDAT-SEN         PIC S9(7)           COMP-3.                  
003600*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
003700     03 IDAVINR-SEN          PIC S9(7)           COMP-3.                  
003800*                                 AVINUMMER SENASTE INLEVERANS            
003900     03 FLPASS               PIC X.                                       
004000     03 BEART                PIC X(25).                                   
004100*                                 ARTIKELBENÄMNING                        
004200     03 BELEV                PIC X(35).                                   
004300*                                 LEVERANTÖRSNAMN                         
004400     03 BELEVART             PIC X(30).                                   
004500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004600     03 ADATTENT             PIC X(40).                                   
004700*                                 ATTENTIONADRESS                         
004800     03 IDLEVFAX             PIC X(20).                                   
004900*                                 TELEFAXNUMMER TILL LEVERANTÖR           
005000     03 IDLEVTLF             PIC X(20).                                   
005100*                                 TELEFONNUMMER TILL LEVERANTÖR           
005200     03 TELEVBSK-INFO        PIC X(10).                                   
005300     03 KVAVROP-NEXT         PIC S9(7)           COMP-3.                  
005400*                                 AVROPSKVANTITET                         
005500     03 DAAVROP-AVS-NEXT     PIC 9(6).                                    
005600*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005700*                                 (ÅÅÅÅVV)                                
005800     03 KVAVROP-LAST         PIC S9(7)           COMP-3.                  
005900*                                 AVROPSKVANTITET                         
006000     03 DAAVROP-AVS-LAST     PIC 9(6).                                    
006100*                                 AVSÄNDNINGSVECKA (PLANERAD)             
006200*                                 (ÅÅÅÅVV)                                
006300     03 DELAYED              OCCURS 10 TIMES.                             
006400        05 KVAVROP-DELAYED   PIC S9(7)           COMP-3.                  
006500*                                 AVROPSKVANTITET                         
006600        05 DAAVROP-AVS-DELAYED                                            
006700                             PIC 9(6).                                    
006800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
006900*                                 (ÅÅÅÅVV)                                
007000*** END OF VILMAII-COPY LENGTH= 361 BYTES                                 
