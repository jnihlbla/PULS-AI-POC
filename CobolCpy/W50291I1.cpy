000100 01  REQU-W50291I1.                                                       
000200*                                 REQUEST-COPYTEXT FÖR BILD 5291          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS DOWNLOAD                            
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 REQU-PRAVCOST        PIC X(10).                                   
000900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001000*                                 AVERAGE COST FOREIGN CURRENCY           
001100     03 REQU-SUARTAVG        PIC X(10).                                   
001200*                                 SUMMA AVERAGECOST                       
001300*                                 SUM OF AVERAGE COST                     
001400     03 REQU-FLNOHAND        PIC X.                                       
001500*                                 INVENTERA INAKTIVA ARTIKLAR?            
001600*                                 INVENTORY INACTIVE PARTS?               
001700     03 REQU-FLBLINDCO       PIC X.                                       
001800*                                 ANVÄND BLIND COUNT?                     
001900*                                 USE BLIND COUNT?                        
002000     03 REQU-KDACS           PIC X.                                       
002100*                                 KÖRNINGSVARIANT FÖR ACS-RUTIN           
002200*                                 PROCESSING VARIANT FOR ACS RTN          
002300     03 REQU-PRAVCOST-DEV1   PIC X(10).                                   
002400*                                 MEDELVÄRDESKST I UTL.VAL OMG.1          
002500*                                 AVERAGE COST FOR. CUR. ROUND 1          
002600     03 REQU-REQTYDEV-1      PIC 9(3).                                    
002700*                                 KVANTITETSAVVIKELSE OMGÅNG 1            
002800*                                 QUANTITY DEVIATION ROUND 1              
002900     03 REQU-PRAVCOST-DEV2   PIC X(10).                                   
003000*                                 MEDELVÄRDESKST I UTL.VAL OMG.2          
003100*                                 AVERAGE COST FOR. CUR. ROUND 2          
003200     03 REQU-REQTYDEV-2      PIC 9(3).                                    
003300*                                 KVANTITETSAVVIKELSE OMGÅNG 2            
003400*                                 QUANTITY DEVIATION ROUND 2              
003500*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
