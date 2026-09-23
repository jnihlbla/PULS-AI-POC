000100 01  MID-W2I36401.                                                        
000200     03 MID-IDARTNR-IN       PIC X(9).                                    
000300*                                 PART NUMBER                             
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 MID-INPUT.                                                        
000700        05 MID-IDLEVNR-SLAG  PIC X(5).                                    
000800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000900        05 MID-FLORDSP       PIC X.                                       
001000*                                 ORDER BLOCKED                           
001100        05 MID-FLSPBULK      PIC X.                                       
001200*                                 FLAG BULKORDER STOP                     
001300        05 MID-KVDAGAR-MANLT PIC 9(3).                                    
001400        05 MID-IDPERSON-BUY  PIC X(3).                                    
001500*                                 REFILL RESPONSIBLE ID                   
001600        05 MID-FLREFERAL     PIC X.                                       
001700*                                 REFERALPART IN COUNTRY                  
001800        05 MID-TEARTNOT-ORDER                                             
001900                             PIC X(70).                                   
002000*                                 PART REMARKS NOTE ORDER                 
002100*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
