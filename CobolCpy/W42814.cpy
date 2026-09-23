000100 01  W42814.                                                              
000200*                                 UPPFÖLJNINGSFIL LEVERANSANM             
000300     03 IDFTG                PIC 9(2).                                    
000400*                                 FÖRETAGSID EKONOM REDOVISNING           
000500*                                 COMPANY IDENTITY ACCOUNTING             
000600     03 IDDC-RET             PIC X(2).                                    
000700*                                 MOTTAGANDE LAGER FÖR RETURER            
000800*                                 RECEIVING WAREHOUSE FOR RETURNS         
000900     03 IDLANDX2             PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100*                                 2-LETTER CODE FOR COUNTRY               
001200     03 ADCITY               PIC X(20).                                   
001300     03 TIAAPP               PIC 9(4).                                    
001400*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
001500*                                 12 PER ÅR                               
001600*                                 NUMERA ÄR DETTA "PV-PERIOD"             
001700*                                 YEAR - PLANNING PERIOD (YYPP)           
001800*                                 12 PER YEAR                             
001900     03 KVRETINL             PIC 9(6).                                    
002000*                                 INLAGT ANTAL VID RETUR                  
002100*                                 RECEIVED QUANTITY ON RETURN             
002200     03 KVRETINL-SKR         PIC 9(6).                                    
002300*                                 INRPT ANTAL SOM SKROTATS                
002400*                                 REPORTED QTY SCRAPPED                   
002500     03 KVAVV-KVANT          PIC 9(7).                                    
002600*                                 ANTALSAVVIKELSE KVANTITET               
002700*                                 QUANTITYDEVIATION QUANTITY              
002800     03 KVDAGDEC             PIC 9(4)V9(1).                               
002900*                                 ANTAL DAGAR MED DECIMAL                 
003000*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
