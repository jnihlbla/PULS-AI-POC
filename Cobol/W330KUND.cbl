000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W330KUND.                                                 
000300 AUTHOR.        SUSANNE ENEGARD.                                          
000400 DATE-WRITTEN.  NOVEMBER 1989.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*    FUNKTION:                                                            
000900*        DENNA MODUL HÄMTAR ALLA DISTRIKTNUMMER SOM FINNS                 
001000*        PÅ KUNDREGISTRET OCH KOMPLETTERAR MED KONCERNNUMMER.             
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 DATA DIVISION.                                                           
001500     EJECT                                                                
001600 WORKING-STORAGE SECTION.                                                 
001700*    -- CHECKED BY WY2000                                                 
001800                                                                          
001900 77  IDPGM                   PIC X(8)    VALUE 'W330KUND'.                
002000 77  JA                      PIC X(1)    VALUE 'J'.                       
002100 77  NEJ                     PIC X(1)    VALUE 'N'.                       
002200 77  INDX                    PIC S9(9)   VALUE +1  COMP SYNC.             
002300                                                                          
002400                                                                          
002500 01  DYNAMISKA-SUBPROGRAM.                                                
002600     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
002700     03  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
002800                                                                          
002900     EJECT                                                                
003000*- - - - -ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
003100*                                                                         
003200 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
003300                                                                          
003400*- - - - -NYCKLAR OCH SÖKFÄLT TILL DLI                                    
003500 01  NYCKLAR-TILL-DLI.                                                    
003600     03  W-IDDISTR-X.                                                     
003700         05  W-IDDISTR       PIC S9(5)   VALUE ZERO  COMP-3.              
003800     03  W-WDB1-WDB101KY-X.                                               
003900         05  W-WDB1-IDPARTNR PIC X(9)    VALUE SPACE.                     
004000         05  W-WDB1-IDFTG    PIC 9(2)    VALUE ZERO.                      
004100                                                                          
004200*- - - - -STATUSKOD FRÅN IMS                                              
004300 01  STATUS-WS               PIC XX.                                      
004400     88  SEGMENT-FINNS                        VALUE '  '.                 
004500     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
004600     88  BASEN-SLUT                           VALUE 'GB'.                 
004700                                                                          
004800 01  GODK-STATUSKODER.                                                    
004900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005000                                                                          
005100 01  SSA1                    PIC X(64).                                   
005200                                                                          
005300     EJECT                                                                
005400*01  -COPY W0003.                                                         
005500                                                                          
005600     EJECT                                                                
005700 01  DLI-IO-AREA.                                                         
005800*  03  WLGMTA01 -COPY WDB201  -PRE GMTA-.                                 
005900     EJECT                                                                
006000 01  DLI-IO-AREA2.                                                        
006100*  03  WLBETC01 -COPY WDB101  -PRE WDB1-.                                 
006200     EJECT                                                                
006300 LINKAGE SECTION.                                                         
006400*01  -COPY W330KUND.                                                      
006500     EJECT                                                                
006600*01  -COPY W0008       -PRE GMTA-.                                        
006700         05  FILLER          PIC X.                                       
006800                                                                          
006900*01  -COPY W0008       -PRE WDB1-.                                        
007000         05  FILLER          PIC X.                                       
007100     EJECT                                                                
007200 PROCEDURE DIVISION USING  KUND-W330KUND GMTA-PCB WDB1-PCB.               
007300 STYR SECTION.                                                            
007400     ENTRY 'DLITCBL' USING KUND-W330KUND GMTA-PCB WDB1-PCB.               
007500                                                                          
007600     MOVE '0' TO KUND-KDSVAR                                              
007700     MOVE +1 TO INDX                                                      
007800     PERFORM IMS-GET-DISTRIKT                                             
007900     PERFORM UNTIL INDX > 1600                                            
008000       IF SEGMENT-FINNS                                                   
008100         MOVE GMTA-GMT-IDDISTR    TO KUND-IDDISTR(INDX)                   
008200                                     W-IDDISTR                            
008300         MOVE GMTA-GMT-IDPARTNR   TO W-WDB1-IDPARTNR                      
008400         MOVE GMTA-GMT-IDFTG      TO W-WDB1-IDFTG                         
008500         MOVE ZERO                TO KUND-KDMARK-BUDG(INDX)               
008600         MOVE SPACE               TO KUND-BEMARK-BUDG(INDX)               
008700         PERFORM IMS-GET-WDB101                                           
008800           IF SEGMENT-FINNS                                               
008900             MOVE WDB1-BET-IDPROMR TO KUND-IDPROMR(INDX)                  
009000           ELSE                                                           
009100             MOVE SPACE            TO KUND-IDPROMR(INDX)                  
009200           END-IF                                                         
009300         PERFORM IMS-GET-DISTRIKT                                         
009400       ELSE                                                               
009500         MOVE +99999              TO KUND-IDDISTR(INDX)                   
009600         MOVE ZERO                TO KUND-KDMARK-BUDG(INDX)               
009700         MOVE SPACE               TO KUND-BEMARK-BUDG(INDX)               
009800         MOVE SPACE               TO KUND-IDPROMR(INDX)                   
009900       END-IF                                                             
010000       ADD +1 TO INDX                                                     
010100     END-PERFORM                                                          
010200                                                                          
010300     MOVE ZERO TO RETURN-CODE                                             
010400     GOBACK                                                               
010500     .                                                                    
010600     EJECT                                                                
010700*   -----   IMS-SEKTIONER                                                 
010800 IMS-GET-DISTRIKT SECTION.                                                
010900                                                                          
011000     STRING 'WLGMTA01(IDDISTR  >' W-IDDISTR-X ')'                         
011100          DELIMITED BY SIZE INTO SSA1                                     
011200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
011300     CALL CBLTDLI USING GN GMTA-PCB DLI-IO-AREA SSA1                      
011400     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
011500     PERFORM IMS-STATUSKONTROLL                                           
011600     .                                                                    
011700     SKIP2                                                                
011800 IMS-GET-WDB101 SECTION.                                                  
011900                                                                          
012000     STRING 'WLBETC01(WDB101KY =' W-WDB1-WDB101KY-X ')'                   
012100          DELIMITED BY SIZE INTO SSA1                                     
012200     MOVE '  GE' TO GODK-STATUSKODER                                      
012300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA2 SSA1                     
012400     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
012500     PERFORM IMS-STATUSKONTROLL                                           
012600     .                                                                    
012700     EJECT                                                                
012800 IMS-STATUSKONTROLL SECTION.                                              
012900                                                                          
013000     SET STATUS-IX TO 1                                                   
013100     SEARCH GODK-STATUS                                                   
013200       AT END                                                             
013300         CALL FELLOG                                                      
013400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
013500         CONTINUE                                                         
013600     END-SEARCH                                                           
013700     .                                                                    
