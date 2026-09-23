000100 01  REF-WDB616.                                                          
000200*                                 DC STYRREGISTER                         
000300*                                 REFILLANDE DC INFO                      
000400*                                 FYSISK NYCKEL: IDDC-REF                 
000500     03 REF-IDDC-REF         PIC X(2).                                    
000600*                                 SÄNDANDE LAGER FÖR REFILL               
000700*                                 SENDING WAREHOUSE FOR REFILL            
000800     03 REF-IDDISTR-REFILL   PIC S9(5)           COMP-3.                  
000900*                                 REFILL DISTRIKT                         
001000*                                 REFILL DISTRICT                         
001100     03 REF-IDDISTR-RETUR    PIC S9(5)           COMP-3.                  
001200*                                 RETUR DISTRIKT                          
001300*                                 RETUR DISTRICT                          
001400     03 REF-IDDISTR-QRETUR   PIC S9(5)           COMP-3.                  
001500*                                 KAVLITET RETUR DISTRIKT                 
001600*                                 QUALITY RETURN DISTRICT                 
001700     03 REF-IDKUNDNR-BPS     PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER FÖR BYPASSORDER              
001900*                                 CUSTOMER NO FOR BYPASSORDER             
002000     03 REF-IDKUNDNR-SBPS    PIC S9(7)           COMP-3.                  
002100*                                 KUND FÖR SNABB BYPASSORDER              
002200*                                 CUSTOMER FOR FAST BYPASSORDER           
002300     03 REF-IDKUNDNR-RETUR   PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER FÖR RETUR                    
002500*                                 CUSTOMER NO FOR RETURN                  
002600     03 REF-IDKUNDNR-QRETUR  PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER FÖR KVALITETSRETUR           
002800*                                 CUSTOMER NO FOR QUALITY RETURN          
002900     03 REF-IDKUNDNR-SRETUR  PIC S9(7)           COMP-3.                  
003000*                                 KUNDNUMMER FÖR SNABBRETUR               
003100*                                 CUSTOMER NO FOR FASTRETURN              
003200     03 REF-IDKUNDNR-TRETUR  PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER FÖR TOTALRETUR               
003400*                                 CUSTOMER NO FOR TOTALRETURN             
003500     03 REF-IDKUNDNR-SQRET   PIC S9(7)           COMP-3.                  
003600*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
003700*                                 CUSTOMER NO FOR FAST QUALRETURN         
003800     03 REF-IDKUNDNR-SORD    PIC S9(7)           COMP-3.                  
003900*                                 KUNDNUMMER FÖR SNABBORDER               
004000*                                 CUSTOMER NO FOR FAST ORDER              
004100     03 REF-KVDLTID-TOT      PIC S9(3)           COMP-3.                  
004200*                                 TOT ANTAL KALENDERDAGAR LEDTID          
004300*                                 TOTAL CALENDAR DAYS LEAD TIME           
004400     03 REF-KVDLTID-BOATPAC  PIC S9(3)           COMP-3.                  
004500*                                 ANTAL ARBDAGAR PACKNINGSTID,BÅT         
004600*                                 NO.WORKDAYS PACKING TIME,  BOAT         
004700     03 REF-KVDLTID-BOATTRP  PIC S9(3)           COMP-3.                  
004800*                                 KALENDERDAGAR TRANSPORTTID, BÅT         
004900*                                 NO.CAL.DAYS TRANSPORT TIME,BOAT         
005000     03 REF-KVDLTID-BOAT2DC  PIC S9(3)           COMP-3.                  
005100*                                 ARB.DAGAR FRÅN HAMN TILL DC,BÅT         
005200*                                 WRK.DAYS  HARBOUR TO DC,   BOAT         
005300     03 REF-KVDLTID-BOATINS  PIC S9(3)           COMP-3.                  
005400*                                 ANTAL ARBDAGAR INLÄGG.TID,  BÅT         
005500*                                 NO.WRKDAYS PUT-IN-STORAGEE,BOAT         
005600     03 REF-KVDLTID-AIRREQ   PIC S9(3)           COMP-3.                  
005700*                                 ANTAL KALENDERDAGAR BEHOV, FLYG         
005800*                                 NO. CALENDAR DAYS PROPOSAL, AIR         
005900     03 REF-KVDLTID-AIRETA   PIC S9(3)           COMP-3.                  
006000*                                 KAL.DAGAR FÖR ANKONSTDATUM FLYG         
006100*                                 CAL.DAYS FOR EST.T.O.ARRIV. AIR         
006200     03 REF-KVDLTID-AIRPAC   PIC S9(3)           COMP-3.                  
006300*                                 ANTAL ARB.DAGAR PACK.TID,  FLYG         
006400*                                 NO. WORKDAYS PACKING TIME,  AIR         
006500     03 REF-KVDLTID-AIRTRP   PIC S9(3)           COMP-3.                  
006600*                                 KALENDERDAGAR TRANSPORTTID,FLYG         
006700*                                 NO. CAL. DAYS TRANSP. TIME, AIR         
006800     03 REF-KVDLTID-AIRINS   PIC S9(3)           COMP-3.                  
006900*                                 ANTAL ARBDAGAR INLÄGGNING, FLYG         
007000*                                 NO.WORK DAYS PUT-IN-STORAGE,AIR         
007100     03 REF-KVDLTID-CUST     PIC S9(3)           COMP-3.                  
007200*                                 ARBETSDAGAR LEDTID I TULLEN             
007300*                                 WORK DAYS LEAD TIME IN CUSTOMS          
007400     03 REF-KVDLTID-CUSTWAIT PIC S9(3)           COMP-3.                  
007500*                                 ARBETSDAGAR VÄNTETID I TULLEN           
007600*                                 WORK.DAYS WAIT TIME IN CUSTOMS          
007700     03 REF-KVDLTID-CUST2DC  PIC S9(3)           COMP-3.                  
007800*                                 ARBETSDAGAR FRÅN TULL TILL DC           
007900*                                 NO.WORK DAYS FROM CUSTOMS TO DC         
008000     03 REF-KVDLTID-BUFF     PIC S9(3)           COMP-3.                  
008100*                                 ARBETSDAGAR LEDTID BUFFER               
008200*                                 WORK DAYS LEAD TIME BUFFER              
008300     03 REF-BETEXT           PIC X(55).                                   
008400     03 REF-TIREFBAT         PIC 9(2).                                    
008500*                                 KLOCKSLAG (TT) FÖR REFILL BATCH         
008600*                                 HOUR (HH) FOR REFILL BATCH              
008700     03 REF-REAIRCO          PIC S9(6)V9(1)      COMP-3.                  
008800*                                 KONSTANT FLYGFRAKT BERÄKNING            
008900*                                 CONSTANT AIR FREIGHT COST               
009000     03 REF-REFILL-DAYS      OCCURS 7 TIMES.                              
009100*                                 VECKODAGSTABELL FÖR ORDERREFILL         
009200*                                 WEEKDAYS TABEL FOR REFILL ORDER         
009300        05 REF-FLREFBLK      PIC X.                                       
009400*                                 VECKO DAGNR FÖR REF. BULKORDER          
009500*                                 WEEKDAY NO. FOR REF. BULK ORDER         
009600        05 REF-FLREFDAY      PIC X.                                       
009700*                                 VECKO DAGNR FÖR REF. DAGORDER           
009800*                                 WEEKDAY NO. FOR REF. DAY ORDER          
009900        05 REF-KDREFDG       PIC X.                                       
010000*                                 VECKO DAGNR FÖR REFILL FG               
010100*                                 WEEK DAY NO. FOR REFILLING DG           
010200     03 REF-FLFRAKDG         PIC X.                                       
010300*                                 FLAGGA FRAKTKOD FARLIGT GODS            
010400*                                 FREIGHT CODE FLAG DANG. GOODS           
010500     03 REF-RESSFAC          PIC S9V9(2)         COMP-3.                  
010600*                                 KONSTANT SÄKERHETSLAGER BERÄKNI         
010700*                                 NG                                      
010800*                                 CONSTANT SAFETY STOCK FACTOR            
010900     03 REF-PRFRAKT          PIC S9(7)           COMP-3.                  
011000*                                 FRAKTKOSTNAD                            
011100*                                 FREIGHT COST                            
011200     03 REF-KDSTEER-CUST     PIC X.                                       
011300*                                 KUNDREGELVÄRK REFILL                    
011400*                                 CUSTOMER FRAMEWORK REFILL               
011500     03 REF-FILLER           PIC X(9).                                    
011600*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
