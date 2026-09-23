000100 01  4490-WDGX4490.                                                       
000200*                                 ORDERPLANNERING/LAGRET                  
000300*                                 FYSISK NYCKEL KY4490:                   
000400*                                 (DARFS + IDPRODNR + IDPLKLST)           
000500*                                                                         
000600     03 4490-DARFS           PIC 9(12).                                   
000700*                                 KLART FÖR TRANSPORT                     
000800*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
000900     03 4490-IDPRODNR        PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200     03 4490-IDPLKLST        PIC S9(3)           COMP-3.                  
001300*                                 PLOCKLISTNUMMER                         
001400*                                 PICKING LIST NUMBER                     
001500     03 4490-IDUSER          PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 4490-KDORDKL         PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000*                                 ORDER CLASS                             
002100     03 4490-TIKLAR          PIC S9(7)           COMP-3.                  
002200*                                 KLARDATUM          (ÅÅMMDD)             
002300*                                 READY DATE        (YYMMDD)              
002400*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
