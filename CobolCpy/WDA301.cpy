000100 01  RET-WDA301.                                                          
000200*                                 SÄNDNINGSREGISTER/RETURER               
000300*                                 FYSISK NYCKEL: WDA301KY:                
000400*                                 (IDDC + DAREGDAT + TIKLOCK)             
000500     03 RET-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RET-DAREGDAT         PIC 9(8).                                    
000900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001000*                                 REGISTRATION DATE (YYYYMMDD)            
001100     03 RET-TIKLOCK          PIC S9(9)           COMP-3.                  
001200*                                 KLOCKSLAG (TTMMSSTH)                    
001300*                                 TIME OF DAY (HHMMSSTH)                  
001400     03 RET-ADINLOMR         PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600*                                 RECEIVING AREA                          
001700     03 RET-ADINLOMR-LOSS    PIC X(4).                                    
001800*                                 LOSSNINGS PLACERING                     
001900*                                 UNLOADING AREA                          
002000     03 RET-ADINLOMR-MOT     PIC X(4).                                    
002100*                                 MOTTAGNINGS PLACERING                   
002200*                                 RECEIVING AREA                          
002300     03 RET-FLFARLIG         PIC X.                                       
002400*                                 FARLIGT GODS-FLAGGA                     
002500*                                 DENGEROUS GOODS FLAG                    
002600     03 RET-IDANSTNR-LOSS    PIC S9(5)           COMP-3.                  
002700*                                 ANSTÄLLNINGSNUMMER LOSSARE              
002800*                                 EMPLOYEE NUMBER UNLOADER                
002900     03 RET-IDANSTNR-MOT     PIC S9(5)           COMP-3.                  
003000*                                 ANSTÄLLNINGSNUMMER MOTTAGARE            
003100*                                 EMPLOYEE NUMBER RECIEVER                
003200     03 RET-IDDISTR          PIC S9(5)           COMP-3.                  
003300*                                 DISTRIKTNUMMER                          
003400*                                 DISTRICT NUMBER                         
003500     03 RET-IDKUNDNR         PIC S9(7)           COMP-3.                  
003600*                                 KUNDNUMMER                              
003700*                                 CUSTOMER NO                             
003800     03 RET-IDRAPPNR         PIC 9(7).                                    
003900*                                 RAPPORT NUMMER                          
004000*                                 DISCREPANCY REPORT NUMBER               
004100     03 RET-IDFRASED-AAF     PIC X(15).                                   
004200*                                 FRAKTSEDELSNUMMER FRÅN ÅTERFÖRS         
004300*                                 ÄLJARE                                  
004400*                                 FREIGHT LETTER NO, FROM DEALER          
004500     03 RET-IDFRASED-CDC     PIC X(15).                                   
004600*                                 FRAKTSEDELSNUMMER TILL CDC              
004700*                                 FREIGHT LETTER NO, TO CDC               
004800     03 RET-IDKOLLI          PIC S9(5)           COMP-3.                  
004900*                                 KOLLINUMMER                             
005000*                                 CASE NUMBER                             
005100     03 RET-IDPERSON         PIC S9(3)           COMP-3.                  
005200*                                 PERSONKOD                               
005300*                                 STAFF CODE                              
005400     03 RET-IDRETSND.                                                     
005500*                                 RETURSÄNDNING                           
005600*                                 RETURN TRANSFER                         
005700        05 RET-IDRT          PIC X(3).                                    
005800*                                 RETURTERMINAL                           
005900*                                 RETURN TERMINAL                         
006000        05 RET-IDRTLOP       PIC 9(3).                                    
006100*                                 RETUR TERMINAL LÖPNUMMER                
006200*                                 RETURN TERMINAL SEQUENCE NUMBER         
006300     03 RET-KDARBTYP         PIC X(8).                                    
006400*                                 TYP AV ARBETE                           
006500*                                 CATEGORY OF WORK                        
006600     03 RET-KDKOLSTA         PIC S9              COMP-3.                  
006700*                                 KOLLISTATUS                             
006800*                                 CASE STATUS                             
006900     03 RET-KDRETSTA         PIC X.                                       
007000*                                 STATUS RETURER                          
007100*                                 RETURN STATUS                           
007200     03 RET-KVKOLLI-AAF      PIC S9(5)           COMP-3.                  
007300*                                 ANTAL KOLLI FRÅN ÅTERFÖRSÄLJARE         
007400*                                 QTY CASES FROM DEALER                   
007500     03 RET-KVKOLLI-LOSS     PIC S9(5)           COMP-3.                  
007600*                                 ANTAL KOLLI SOM LOSSAS                  
007700*                                 QTY CASES UNLOADED                      
007800     03 RET-KVKOLLI-MOT      PIC S9(5)           COMP-3.                  
007900*                                 ANTAL MOTTAGNA KOLLIN                   
008000*                                 QUANTITY CASES RECEIVED                 
008100     03 RET-KVRADER          PIC S9(5)           COMP-3.                  
008200*                                 ANTAL RADER                             
008300*                                 NUMBER OF LINES                         
008400     03 RET-TERETNOT         PIC X(20).                                   
008500*                                 FRI NOTERING RETURER                    
008600*                                 FREE TEXT FIELD DISCREPANCIES           
008700     03 RET-TIINLMOT         PIC S9(7)           COMP-3.                  
008800*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
008900*                                 RECEIVING DATE    (YYMMDD)              
009000     03 RET-TIKLAR           PIC S9(7)           COMP-3.                  
009100*                                 KLARDATUM          (ÅÅMMDD)             
009200*                                 READY DATE        (YYMMDD)              
009300     03 RET-TILOSSN          PIC S9(7)           COMP-3.                  
009400*                                 LOSSNINGSDATUM                          
009500*                                 DATE OF UNLOADING                       
009600     03 RET-DASNDDAT         PIC 9(8).                                    
009700*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
009800*                                 SHIPPING DATE   (YYYYMMDD)              
009900     03 RET-DARETANK         PIC 9(8).                                    
010000*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
010100*                                 DATE GOODS RECEIVING(YYYYMMDD)          
010200     03 RET-IDRT-TRANSIT     PIC X(3).                                    
010300*                                 TRANSIT RETURTERMINAL                   
010400*                                 TRANSIT RETURN TERMINAL                 
010500     03 RET-TIREGDAT-TRRT    PIC S9(7)           COMP-3.                  
010600*                                 REG. DATUM I TRANSIT RET.TERM.          
010700*                                 REGISTRATION DATE ON TRANSIT RT         
010800     03 RET-TISNDDAT-TRRT    PIC S9(7)           COMP-3.                  
010900*                                 SÄND. DATUM FRÅN TRANSIT RT             
011000*                                 SEND DATE FROM TRANSIT RT               
011100     03 RET-FLBUYBAC         PIC X.                                       
011200*                                 FLAGGA BUYBACK J/N                      
011300*                                 BUYBACK FLAG J/N                        
011400     03 RET-FILLER           PIC X(9).                                    
011500*** END OF VILMAII-COPY LENGTH= 180 BYTES                                 
