000100 01  W42816.                                                              
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
001300     03 TIAAVV               PIC 9(4).                                    
001400*                                 ÅR - VECKA  (ÅÅVV)                      
001500*                                 YEAR - WEEK  (YYWW)                     
001600     03 KVRETINL             PIC 9(6).                                    
001700*                                 INLAGT ANTAL VID RETUR                  
001800*                                 RECEIVED QUANTITY ON RETURN             
001900     03 KVRETINL-SKR         PIC 9(6).                                    
002000*                                 INRPT ANTAL SOM SKROTATS                
002100*                                 REPORTED QTY SCRAPPED                   
002200     03 KVAVV-KVANT          PIC 9(7).                                    
002300*                                 ANTALSAVVIKELSE KVANTITET               
002400*                                 QUANTITYDEVIATION QUANTITY              
002500     03 KVDAGDEC             PIC 9(4)V9(1).                               
002600*                                 ANTAL DAGAR MED DECIMAL                 
002700*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
