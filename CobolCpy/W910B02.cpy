000100 01  W910B02-CTX.                                                         
000200*                                 POST FÖR AVSTÄMNING ORDERLÄGE           
000300*                                 RS   VR.                                
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDSUPPL              PIC 9(4).                                    
001200*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001300     03 REKSUPPL             PIC 9.                                       
001400*                                 LEVERANTÖRENS KONTROLLSIFFRA            
001500     03 IDORDNR7             PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 IDARTNR              PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 REKSIFFR             PIC 9.                                       
002000*                                 KONTROLLSIFFRA                          
002100     03 KVBEART              PIC 9(6).                                    
002200*                                 BESTÄLLT ANTAL STYCKEN                  
002300     03 TIAAMMDD             PIC 9(6).                                    
002400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002500     03 KDORDER              PIC 9.                                       
002600*                                 ORDERKOD                                
002700     03 KDRO                 PIC 9.                                       
002800*                                 RESTORDERKOD PÅ INFORMATION             
002900*                                 TILL VR                                 
003000     03 KDVRINFO             PIC 9.                                       
003100*                                 PÅVERKAN I VR/DSP SYSTEM                
003200*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
