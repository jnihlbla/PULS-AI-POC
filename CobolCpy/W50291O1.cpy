000100 01  RESP-W50291O1.                                                       
000200*                                 RESPONSE-COPYTXT FÖR BILD 5291          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS DOWNLOAD                            
000500     03 RESP-PRAVCOST        PIC Z(6)9.9(2).                              
000600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
000700*                                 AVERAGE COST FOREIGN CURRENCY           
000800     03 RESP-SUARTAVG        PIC Z(6)9.9(2).                              
000900*                                 SUMMA AVERAGECOST                       
001000*                                 SUM OF AVERAGE COST                     
001100     03 RESP-FLNOHAND        PIC X.                                       
001200*                                 INVENTERA INAKTIVA ARTIKLAR?            
001300*                                 INVENTORY INACTIVE PARTS?               
001400     03 RESP-FLBLINDCO       PIC X.                                       
001500*                                 ANVÄND BLIND COUNT?                     
001600*                                 USE BLIND COUNT?                        
001700     03 RESP-KDACS           PIC X.                                       
001800*                                 KÖRNINGSVARIANT FÖR ACS-RUTIN           
001900*                                 PROCESSING VARIANT FOR ACS RTN          
002000     03 RESP-PRAVCOST-DEV1   PIC Z(6)9.9(2).                              
002100*                                 MEDELVÄRDESKST I UTL.VAL OMG.1          
002200*                                 AVERAGE COST FOR. CUR. ROUND 1          
002300     03 RESP-REQTYDEV-1      PIC Z(2)9.                                   
002400*                                 KVANTITETSAVVIKELSE OMGÅNG 1            
002500*                                 QUANTITY DEVIATION ROUND 1              
002600     03 RESP-PRAVCOST-DEV2   PIC Z(6)9.9(2).                              
002700*                                 MEDELVÄRDESKST I UTL.VAL OMG.2          
002800*                                 AVERAGE COST FOR. CUR. ROUND 2          
002900     03 RESP-REQTYDEV-2      PIC Z(2)9.                                   
003000*                                 KVANTITETSAVVIKELSE OMGÅNG 2            
003100*                                 QUANTITY DEVIATION ROUND 2              
003200*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
