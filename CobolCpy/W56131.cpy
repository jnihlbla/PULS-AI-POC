000100 01  W56131.                                                              
000200*                                 FIELDS FROM WDL601 AND WDL611           
000300*                                                                         
000400     03 IDLANDX2             PIC X(2).                                    
000500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000600*                                 2-LETTER CODE FOR COUNTRY               
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300*                                 SERIAL NO RECEIVING REPORT              
001400*                                 (0WWDLLLLC)                             
001500     03 IDLEVNR              PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400     03 KVAVIS               PIC S9(7)           COMP-3.                  
002500*                                 AVISERAT ANTAL                          
002600*                                 QUANTITY NOTIFIED                       
002700     03 KVANTMOT             PIC S9(7)           COMP-3.                  
002800*                                 ANTAL MOTTAGET                          
002900*                                 QUANTITY RECEIVED                       
003000     03 TIINLINL             PIC S9(7)           COMP-3.                  
003100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003200*                                 DATE OF REPORTED IN STOCK (R32)         
003300     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 ARTIKELPRIS NETTO                       
003500*                                 NET PRICE EACH   (FOB NET)              
003600     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
003700*                                 VALUTAKURS                              
003800*                                 CURRENCY EXCHANGE RATE                  
003900     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
004000*                                 AVISERINGSDATUM (YYMMDD)                
004100*                                 ADVICE NOTE DATE                        
004200*** END OF VILMAII-COPY LENGTH= 56 BYTES                                  
