000100 01  MOD-W4O40701.                                                        
000200*                                 COPYTEXT FOR MOD W4O40701               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 MOD-TID-TRS          PIC X(2).                                    
001200*                                 WEEKDAY AUTOM.TRANSF,RET,SCRAP          
001300     03 MOD-TID-TRS-IN-ATTR  PIC X(2).                                    
001400     03 MOD-TID-TRS-IN       PIC X(2).                                    
001500*                                 WEEKDAY AUTOM.TRANSF,RET,SCRAP          
001600     03 MOD-TIVV             PIC 9(2).                                    
001700*                                 WEEK   (WW)                             
001800     03 MOD-TIVV-IN-ATTR     PIC X(2).                                    
001900     03 MOD-TIVV-IN          PIC 9(2).                                    
002000*                                 WEEK   (WW)                             
002100     03 MOD-KVVECKOR-UT      PIC Z(2)9.                                   
002200*                                 NO. OF WEEKS FOR DESTOCKING             
002300     03 MOD-KVVECKOR-IN-ATTR PIC X(2).                                    
002400     03 MOD-KVVECKOR-IN      PIC Z(2)9.                                   
002500*                                 NO. OF WEEKS FOR DESTOCKING             
002600     03 MOD-FLTRANS-PAS      PIC X.                                       
002700*                                 TRANSFER PASSIVE PART OK?               
002800     03 MOD-FLTRANS-PAS-IN-ATTR                                           
002900                             PIC X(2).                                    
003000     03 MOD-FLTRANS-PAS-IN   PIC X.                                       
003100*                                 TRANSFER PASSIVE PART OK?               
003200     03 MOD-IDDC-TPAS-1      PIC X(2).                                    
003300*                                 DC 1 FOR TRANSFER OF PASS PART          
003400     03 MOD-IDDC-TPAS-1-IN-ATTR                                           
003500                             PIC X(2).                                    
003600     03 MOD-IDDC-TPAS-1-IN   PIC X(2).                                    
003700*                                 DC 1 FOR TRANSFER OF PASS PART          
003800     03 MOD-IDDC-TPAS-2      PIC X(2).                                    
003900*                                 DC 2 FOR TRANSFER OF PASS PART          
004000     03 MOD-IDDC-TPAS-2-IN-ATTR                                           
004100                             PIC X(2).                                    
004200     03 MOD-IDDC-TPAS-2-IN   PIC X(2).                                    
004300*                                 DC 2 FOR TRANSFER OF PASS PART          
004400     03 MOD-IDDC-TPAS-3      PIC X(2).                                    
004500*                                 DC 3 FOR TRANSFER OF PASS PART          
004600     03 MOD-IDDC-TPAS-3-IN-ATTR                                           
004700                             PIC X(2).                                    
004800     03 MOD-IDDC-TPAS-3-IN   PIC X(2).                                    
004900*                                 DC 3 FOR TRANSFER OF PASS PART          
005000     03 MOD-SUARTMIN-TPAS    PIC Z(5)9.                                   
005100*                                 MIN STOCK VALUE FOR TRANSFER            
005200*                                 OF PASSIVE PART                         
005300     03 MOD-SUARTMIN-TPAS-IN-ATTR                                         
005400                             PIC X(2).                                    
005500     03 MOD-SUARTMIN-TPAS-IN PIC 9(6).                                    
005600*                                 MIN STOCK VALUE FOR TRANSFER            
005700*                                 OF PASSIVE PART                         
005800     03 MOD-KVPERIOD-TPAS    PIC Z9.                                      
005900*                                 NBR OF PERIODS WHEN COMPUTING           
006000*                                 TRANSFER OF PASSIVE PART                
006100     03 MOD-KVPERIOD-TPAS-IN-ATTR                                         
006200                             PIC X(2).                                    
006300     03 MOD-KVPERIOD-TPAS-IN PIC X(2).                                    
006400*                                 NBR OF PERIODS WHEN COMPUTING           
006500*                                 TRANSFER OF PASSIVE PART                
006600     03 MOD-KVVECKOR-TPAS    PIC Z(2)9.                                   
006700*                                 NO. WEEKS MONITOR ORDER STATS           
006800*                                 DURING TRANSFER OF PASSIVE PART         
006900     03 MOD-KVVECKOR-TPAS-IN-ATTR                                         
007000                             PIC X(2).                                    
007100     03 MOD-KVVECKOR-TPAS-IN PIC Z(3).                                    
007200*                                 NO. WEEKS MONITOR ORDER STATS           
007300*                                 DURING TRANSFER OF PASSIVE PART         
007400     03 MOD-FLTRANS-ERS      PIC X.                                       
007500*                                 TRANSFER OK FOR SOME SUPERSESS?         
007600     03 MOD-FLTRANS-ERS-IN-ATTR                                           
007700                             PIC X(2).                                    
007800     03 MOD-FLTRANS-ERS-IN   PIC X.                                       
007900*                                 TRANSFER OK FOR SOME SUPERSESS?         
008000     03 MOD-KVPB-LIM         PIC Z(5)9.9.                                 
008100*                                 FORECAST LIMIT                          
008200     03 MOD-KVPB-LIM-IN-ATTR PIC X(2).                                    
008300     03 MOD-KVPB-LIM-IN      PIC X(8).                                    
008400*                                 FORECAST LIMIT                          
008500     03 MOD-SUVARLIM-TPAS    PIC Z(5)9.                                   
008600*                                 VALUE LIMIT PASSIVE TRANSFER            
008700     03 MOD-SUVARLIM-TPAS-IN-ATTR                                         
008800                             PIC X(2).                                    
008900     03 MOD-SUVARLIM-TPAS-IN PIC X(6).                                    
009000*                                 VALUE LIMIT PASSIVE TRANSFER            
009100     03 MOD-FLRETUR-PAS      PIC X.                                       
009200*                                 RETURN OK FOR PASSIVE PART?             
009300     03 MOD-FLRETUR-PAS-IN-ATTR                                           
009400                             PIC X(2).                                    
009500     03 MOD-FLRETUR-PAS-IN   PIC X.                                       
009600*                                 RETURN OK FOR PASSIVE PART?             
009700     03 MOD-SUARTMIN-RPAS    PIC Z(5)9.                                   
009800*                                 MIN STOCK VALUE FOR RETURN              
009900*                                 OF PASSIVE PART                         
010000     03 MOD-SUARTMIN-RPAS-IN-ATTR                                         
010100                             PIC X(2).                                    
010200     03 MOD-SUARTMIN-RPAS-IN PIC 9(6).                                    
010300*                                 MIN STOCK VALUE FOR RETURN              
010400*                                 OF PASSIVE PART                         
010500     03 MOD-KVVECKOR-RPAS    PIC Z9.                                      
010600*                                 NO. WEEKS MONITOR ORDER STATS           
010700*                                 DURING RETURN OF PASSIVE PART           
010800     03 MOD-KVVECKOR-RPAS-IN-ATTR                                         
010900                             PIC X(2).                                    
011000     03 MOD-KVVECKOR-RPAS-IN PIC X(2).                                    
011100*                                 NO. WEEKS MONITOR ORDER STATS           
011200*                                 DURING RETURN OF PASSIVE PART           
011300     03 MOD-FLSKROT-PAS      PIC X.                                       
011400*                                 OK TO SCRAP PASSIVE PART?               
011500     03 MOD-FLSKROT-PAS-IN-ATTR                                           
011600                             PIC X(2).                                    
011700     03 MOD-FLSKROT-PAS-IN   PIC X.                                       
011800*                                 OK TO SCRAP PASSIVE PART?               
011900     03 MOD-KVSKROT-SPAS     PIC Z(2)9.                                   
012000*                                 MAX LIMIT FOR SCRAPING OF PARTS         
012100     03 MOD-KVSKROT-SPAS-IN-ATTR                                          
012200                             PIC X(2).                                    
012300     03 MOD-KVSKROT-SPAS-IN  PIC Z(2)9.                                   
012400*                                 MAX LIMIT FOR SCRAPING OF PARTS         
012500     03 MOD-KVVECKOR-SPAS    PIC Z(2)9.                                   
012600*                                 WEEKS FOR SCRAP PASSIVE PART            
012700     03 MOD-KVVECKOR-SPAS-IN-ATTR                                         
012800                             PIC X(2).                                    
012900     03 MOD-KVVECKOR-SPAS-IN PIC X(3).                                    
013000*                                 NUMBER OF WEEKS                         
013100     03 MOD-IDTECKEN-SPAS    PIC X.                                       
013200*                                 SIGN (>,=,<) FOR SCRAPING PARTS         
013300     03 MOD-IDTECKEN-SPAS-IN-ATTR                                         
013400                             PIC X(2).                                    
013500     03 MOD-IDTECKEN-SPAS-IN PIC X.                                       
013600*                                 SIGN (>,=,<) FOR SCRAPING PARTS         
013700     03 MOD-PRARTSTD-SPAS    PIC Z(4)9.                                   
013800*                                 PRICE LIMIT FOR SCRAPING PARTS          
013900     03 MOD-PRARTSTD-SPAS-IN-ATTR                                         
014000                             PIC X(2).                                    
014100     03 MOD-PRARTSTD-SPAS-IN PIC 9(5).                                    
014200*                                 PRICE LIMIT FOR SCRAPING PARTS          
014300     03 MOD-ADLAGOMR-SPAS    PIC Z9.                                      
014400*                                 AREA FOR SCRAP PASSIVE PARTS            
014500     03 MOD-ADLAGOMR-SPAS-IN-ATTR                                         
014600                             PIC X(2).                                    
014700     03 MOD-ADLAGOMR-SPAS-IN PIC 9(2).                                    
014800*                                 AREA FOR SCRAP PASSIVE PARTS            
014900     03 MOD-IDPERSON-SPAS    PIC Z(2)9.                                   
015000*                                 STAFF SCRAPING PASSIVE PARTS            
015100     03 MOD-IDPERSON-SPAS-IN-ATTR                                         
015200                             PIC X(2).                                    
015300     03 MOD-IDPERSON-SPAS-IN PIC X(3).                                    
015400*                                 STAFF SCRAPING PASSIVE PARTS            
015500     03 MOD-KDPRODSL-SPAS    PIC 9(2).                                    
015600*                                 PRODUCT GR SCRAP PASSIVE PART           
015700     03 MOD-KDPRODSL-SPAS-IN-ATTR                                         
015800                             PIC X(2).                                    
015900     03 MOD-KDPRODSL-SPAS-IN PIC 9(2).                                    
016000*                                 PRODUCT GR SCRAP PASSIVE PART           
016100     03 MOD-TEMFSINF         PIC X(55).                                   
016200*                                 INFORMATION MESSAGE                     
016300*** END OF VILMAII-COPY LENGTH= 289 BYTES                                 
