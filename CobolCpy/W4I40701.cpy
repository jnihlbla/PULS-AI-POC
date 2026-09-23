000100 01  MID-W4I40701.                                                        
000200*                                 COPYTEXT FOR MID W4I40701               
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 WAREHOUSE IDENTIFIER                    
000500     03 MID-TID-TRS          PIC X(2).                                    
000600*                                 WEEKDAY AUTOM.TRANSF,RET,SCRAP          
000700     03 MID-TIVV             PIC 9(2).                                    
000800*                                 WEEK   (WW)                             
000900     03 MID-KVVECKOR-IN      PIC 9(3).                                    
001000*                                 NO. OF WEEKS FOR DESTOCKING             
001100     03 MID-FLTRANS-PAS      PIC X.                                       
001200*                                 TRANSFER PASSIVE PART OK?               
001300     03 MID-IDDC-TPAS-1      PIC X(2).                                    
001400*                                 DC 1 FOR TRANSFER OF PASS PART          
001500     03 MID-IDDC-TPAS-2      PIC X(2).                                    
001600*                                 DC 2 FOR TRANSFER OF PASS PART          
001700     03 MID-IDDC-TPAS-3      PIC X(2).                                    
001800*                                 DC 3 FOR TRANSFER OF PASS PART          
001900     03 MID-SUARTMIN-TPAS    PIC 9(6).                                    
002000*                                 MIN STOCK VALUE FOR TRANSFER            
002100*                                 OF PASSIVE PART                         
002200     03 MID-KVPERIOD-TPAS    PIC 9(2).                                    
002300*                                 NBR OF PERIODS WHEN COMPUTING           
002400*                                 TRANSFER OF PASSIVE PART                
002500     03 MID-KVVECKOR-TPAS    PIC 9(3).                                    
002600*                                 NO. WEEKS MONITOR ORDER STATS           
002700*                                 DURING TRANSFER OF PASSIVE PART         
002800     03 MID-FLTRANS-ERS      PIC X.                                       
002900*                                 TRANSFER OK FOR SOME SUPERSESS?         
003000     03 MID-KVPB-LIM         PIC X(8).                                    
003100*                                 FORECAST LIMIT                          
003200     03 MID-SUVARLIM-TPAS    PIC 9(6).                                    
003300*                                 VALUE LIMIT PASSIVE TRANSFER            
003400     03 MID-FLRETUR-PAS      PIC X.                                       
003500*                                 RETURN OK FOR PASSIVE PART?             
003600     03 MID-SUARTMIN-RPAS    PIC 9(6).                                    
003700*                                 MIN STOCK VALUE FOR RETURN              
003800*                                 OF PASSIVE PART                         
003900     03 MID-KVVECKOR-RPAS    PIC 9(2).                                    
004000*                                 NO. WEEKS MONITOR ORDER STATS           
004100*                                 DURING RETURN OF PASSIVE PART           
004200     03 MID-FLSKROT-PAS      PIC X.                                       
004300*                                 OK TO SCRAP PASSIVE PART?               
004400     03 MID-KVSKROT-SPAS     PIC 9(3).                                    
004500*                                 MAX LIMIT FOR SCRAPING OF PARTS         
004600     03 MID-KVVECKOR-SPAS    PIC 9(3).                                    
004700*                                 WEEKS FOR SCRAP PASSIVE PART            
004800     03 MID-IDTECKEN-SPAS    PIC X.                                       
004900*                                 SIGN (>,=,<) FOR SCRAPING PARTS         
005000     03 MID-PRARTSTD-SPAS    PIC 9(5).                                    
005100*                                 PRICE LIMIT FOR SCRAPING PARTS          
005200     03 MID-ADLAGOMR-SPAS    PIC 9(2).                                    
005300*                                 AREA FOR SCRAP PASSIVE PARTS            
005400     03 MID-IDPERSON-SPAS    PIC 9(3).                                    
005500*                                 STAFF SCRAPING PASSIVE PARTS            
005600     03 MID-KDPRODSL-SPAS    PIC 9(2).                                    
005700*                                 PRODUCT GR SCRAP PASSIVE PART           
005800*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
