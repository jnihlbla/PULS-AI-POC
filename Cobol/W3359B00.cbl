001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W3359B00.                                                
001600 AUTHOR.         SUSANNE OLSSON                                           
001700 DATE-WRITTEN.   04/12/01.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
001910*                                                                         
001960*                                                                         
002000*    FUNKTION:                                                            
002100*        BMP FÖR ATT SÄNDA ALLA PRISFRÅGOR FÖR SVERIGE SOM INTE           
002200*        KRÄVER SVAR DIREKT I PGM W3039100. ADDISPABS = SPACE.            
002210*        FÅGORNA BUNTAS IHOP FÖR ATT VIPS INTE KLARAR AV ATT TA           
002230*        EMOT SÅ STORA MÄNGDER FRÅGOR SOM DET BLIR FÖR SVERIGE.           
002900*        UPPDATERAR WDC7 MED SEND-DATUM.                                  
002910*                                                                         
002920*    E'TRACKER ID: 1574010                                                
002921*                : 2640783  (DELA PGM I 2 PGM)                            
002930*                                                                         
003000*    UTDATA.                                                              
003100*        SÄNDNING VIA WZ01  TILL VIPS                                     
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003510 INPUT-OUTPUT SECTION.                                                    
003520 FILE-CONTROL.                                                            
003530*     --- INFIL FRÅN W3359C                                               
003540     SELECT W3359C               ASSIGN TO W3359BD1.                      
003550     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003710 FILE SECTION.                                                            
003720 FD  W3359C                                                               
003730     RECORDING F                                                          
003740     BLOCK CONTAINS 0.                                                    
003750                                                                          
003760*01  POST  -COPY W3359C01   -PRE IN-   -L.                                
003770                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W3359B00'.            
004000*                                                                         
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 77  ANTAL-SEND                  PIC S9(4)  COMP-3 VALUE ZERO.            
004520 77  MAX-ANTAL-SEND              PIC S9(4)  COMP-3 VALUE +0400.           
004530 77  W-FIRST-TIME                PIC 9       VALUE ZERO.                  
004600                                                                          
004900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005000     88  NYCKLAR-OK                          VALUE 'J'.                   
005100     88  NYCKLAR-FEL                         VALUE 'N'.                   
005200                                                                          
005201 77  W3359C-EOF-SW               PIC X       VALUE 'N'.                   
005202     88  END-OF-W3359C                       VALUE 'J'.                   
005203                                                                          
005204                                                                          
005205 01  CHKP-VAR.                                                            
005206     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005207     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005208     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005209     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005210     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005211     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005212     SKIP2                                                                
005213                                                                          
005300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005800     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005900     EJECT                                                                
006000 01  MESSAGE-CODES.                                                       
006200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006400     EJECT                                                                
006410*    --- PARAMETRAR TILL POSTSUM                                          
006420*                                                                         
006430*01  -COPY W0005   -PRE  POSTSUM-                                         
006440     EJECT                                                                
006500*    --- AREOR FÖR KOMMUNIKATION                                          
007100 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
007300*01  -COPY WZ01SEND                                                       
007310 01  SEND-DATA.                                                           
007320*03  -COPY WZ01REQU -PRE MID-                                             
007321*03  -COPY W30391O1                                                       
007330 01   KDRC-DISPLAY               PIC X(4).                                
007400     EJECT                                                                
007410*01  FILLER    -COPY W3359C01   -PRE IN-                                  
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008018                                                                          
008019     03  W-WDC701-X.                                                      
008020         05  W-IDDISTR           PIC 9(4)    VALUE ZERO.                  
008030         05  W-IDKUNDNR          PIC 9(7)    VALUE ZERO.                  
008031         05  W-IDBUNDLE          PIC X(15)   VALUE SPACE.                 
008032                                                                          
008033     03  W-WDC711-X.                                                      
008034       05  W-IDPRQUES            PIC 9(7)    VALUE ZERO.                  
008035                                                                          
008036   03  W-IDBUNDLE-X.                                                      
008037     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
008038     05  W-PRQ-IDORDNR7-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
008039       07  W-PRQ-IDORDNR7        PIC 9(7).                                
008040       07  FILLER                PIC X(8).                                
008041     05  W-PRQ-IDRAPPNR-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
008042       07  W-PRQ-IDRAPPNR        PIC 9(7).                                
008043       07  FILLER                PIC X(8).                                
008044                                                                          
008045     03  WDR401-X.                                                        
008046         05  FILLER              PIC X(4)    VALUE '3101'.                
008050         05  W-IDDISTR1          PIC 9(4)    VALUE ZERO.                  
008060         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008610     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008620     88  IMS-EJ-OK                           VALUE 'XD'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(128).                              
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900                                                                          
010003 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
010004 01  DLI-IO-WDC701.                                                       
010005*    03  -COPY WDC701                                                     
010006     EJECT                                                                
010007 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
010008 01  DLI-IO-WDC711.                                                       
010010*    03  -COPY WDC711                                                     
010300     EJECT                                                                
010310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3102'.                    
010370 01  DLI-IO-WDGX3102.                                                     
010380*    03  -COPY WDGX3102                                                   
010394     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010802*01  -COPY W0009  -PRE MSG-                                               
010803*01  -COPY W0008  -PRE WDC7-                                              
010810     05  FILLER                  PIC X.                                   
010820*01  -COPY W0008  -PRE WDR4-                                              
010830     05  FILLER                  PIC X.                                   
010900     EJECT                                                                
011001 PROCEDURE DIVISION  USING MSG-PCB WDC7-PCB WDR4-PCB.                     
011021 MAIN SECTION.                                                            
011022     ENTRY 'DLITCBL' USING MSG-PCB WDC7-PCB WDR4-PCB.                     
011023                                                                          
011026     PERFORM A-INIT                                                       
011028     PERFORM C-CHECKBASE                                                  
011029     PERFORM Z-FINIT                                                      
011046                                                                          
011047     MOVE ZERO TO RETURN-CODE                                             
011048     GOBACK                                                               
011049     .                                                                    
011050     EJECT                                                                
011051                                                                          
011052 A-INIT SECTION.                                                          
011053                                                                          
011054     OPEN INPUT W3359C                                                    
011055     PERFORM IMS-RESTART                                                  
011056                                                                          
011057     MOVE +0                          TO CHKP-ANT                         
011058                                                                          
011059     MOVE +0                          TO ANTAL-SEND                       
011060     MOVE 0                           TO W-FIRST-TIME                     
011061                                                                          
011066     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011067     .                                                                    
011068     EJECT                                                                
011070                                                                          
011128 C-CHECKBASE SECTION.                                                     
011132                                                                          
011133     PERFORM S01-LAES-W3359C                                              
011134     MOVE IN-IDDISTR    TO W-IDDISTR1                                     
011135     IF NOT END-OF-W3359C                                                 
011136       PERFORM IMS-GU-WDGX3102                                            
011137       MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                        
011138     END-IF                                                               
011139                                                                          
011140     PERFORM UNTIL END-OF-W3359C                                          
011150                                                                          
011160       MOVE IN-IDDISTR    TO   W-IDDISTR                                  
011161       MOVE IN-IDKUNDNR   TO   W-IDKUNDNR                                 
011162       MOVE IN-IDBUNDLE   TO   W-IDBUNDLE                                 
011163       PERFORM IMS-GU-WDC701                                              
011165        MOVE PRQ-IDDISTR   TO   MOD-IDDISTR                               
011166        MOVE PRQ-IDKUNDNR  TO   MOD-IDKUNDNR                              
011167        MOVE PRQ-IDBUNDLE  TO   MOD-IDBUNDLE                              
011168       IF SEGMENT-FINNS                                                   
011169        IF IN-IDDISTR NOT = W-IDDISTR1                                    
011170                                                                          
011171          IF W-FIRST-TIME = +1                                            
011172            PERFORM S06-SEND-CLOSE                                        
011173            MOVE 0    TO W-FIRST-TIME                                     
011174            MOVE ZERO TO ANTAL-SEND                                       
011175            PERFORM X-TAG-CHECKPOINT                                      
011176            ADD +1       TO CHKP-ANT                                      
011177          END-IF                                                          
011178                                                                          
011179          MOVE IN-IDDISTR    TO W-IDDISTR1                                
011180          PERFORM IMS-GU-WDGX3102                                         
011181          MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                     
011182        END-IF                                                            
011183                                                                          
011184        PERFORM UNTIL END-OF-W3359C OR                                    
011185              MOD-IDDISTR  NOT = IN-IDDISTR OR                            
011186              MOD-IDKUNDNR NOT = IN-IDKUNDNR OR                           
011187              MOD-IDBUNDLE NOT = IN-IDBUNDLE                              
011188          MOVE IN-IDPRQUES   TO   W-IDPRQUES                              
011189          PERFORM IMS-GHNP-WDC711                                         
011190                                                                          
011191          IF SEGMENT-FINNS                                                
011192            IF W-FIRST-TIME = 0                                           
011193              MOVE 1 TO W-FIRST-TIME                                      
011194              PERFORM S04-SEND-OPEN                                       
011195            END-IF                                                        
011200                                                                          
011207            MOVE LPRQ-IDPRQUES TO   MOD-IDPRQUES                          
011208            MOVE LPRQ-IDARTNR  TO   MOD-IDARTNR                           
011209            MOVE LPRQ-KDORDKL  TO   MOD-KDORDKL                           
011210            MOVE LPRQ-KVBEART  TO   MOD-KVBEART                           
011211                                                                          
011216            PERFORM S05-SEND-PUT                                          
011217                                                                          
011218            ADD +1             TO ANTAL-SEND                              
011219                                                                          
011220            MOVE FUNCTION CURRENT-DATE(1:14) TO                           
011221                                      LPRQ-DADATTID-SEND                  
011222            IF LPRQ-KDORDKL = 4                                           
011223              MOVE '4'                TO LPRQ-FILLER(1:1)                 
011224            END-IF                                                        
011225            PERFORM IMS-REPL-WDC711                                       
011227                                                                          
011230            IF ANTAL-SEND > MAX-ANTAL-SEND                                
011231              PERFORM S06-SEND-CLOSE                                      
011232              MOVE 0    TO W-FIRST-TIME                                   
011233              MOVE ZERO TO ANTAL-SEND                                     
011234              PERFORM X-TAG-CHECKPOINT                                    
011235              ADD +1       TO CHKP-ANT                                    
011236            END-IF                                                        
011237          END-IF                                                          
011238                                                                          
011239          PERFORM S01-LAES-W3359C                                         
011240                                                                          
011241        END-PERFORM                                                       
011242       ELSE                                                               
011243         PERFORM S01-LAES-W3359C                                          
011244       END-IF                                                             
011245                                                                          
011246     END-PERFORM                                                          
011247                                                                          
011248     IF W-FIRST-TIME = 1                                                  
011249       PERFORM S06-SEND-CLOSE                                             
011250     END-IF                                                               
011251                                                                          
011252     .                                                                    
011260     EJECT                                                                
011261 Z-FINIT SECTION.                                                         
011262                                                                          
011265     CLOSE W3359C                                                         
011267                                                                          
011268     MOVE 'S' TO POSTSUM-OPKOD                                            
011269     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
011270     .                                                                    
011271     EJECT                                                                
011272 S01-LAES-W3359C  SECTION.                                                
011273                                                                          
011274     READ W3359C INTO IN-AREA                                             
011275     AT END                                                               
011276        SET END-OF-W3359C TO TRUE                                         
011277     NOT AT END                                                           
011278        MOVE 'W3359C'       TO POSTSUM-FDNAMN                             
011279        MOVE 'W3359BD1'     TO POSTSUM-DDNAMN2                            
011280        MOVE SPACE          TO POSTSUM-TRANSTYP                           
011281        CALL POSTSUM USING POSTSUM-PARM                                   
011282     END-READ                                                             
011283     .                                                                    
011284     EJECT                                                                
011285 S04-SEND-OPEN SECTION.                                                   
011286     MOVE 'OPEN' TO SEND-KDFUNC                                           
011287     CALL WZ01SEND USING SEND-CONTROL-AREA                                
011288                         SEND-OPEN-AREA                                   
011289******** OM FEL                                                           
011290     IF SEND-KDRC  > 0                                                    
011291        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
011292        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
011293        DELIMITED BY SIZE INTO FELTEXT                                    
011294        DISPLAY FELTEXT                                                   
011295        CALL FELLOG                                                       
011296     END-IF                                                               
011297     .                                                                    
011298     EJECT                                                                
011299 S05-SEND-PUT SECTION.                                                    
011300                                                                          
011301     MOVE 'PUT' TO SEND-KDFUNC                                            
011302     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
011303     CALL WZ01SEND USING SEND-CONTROL-AREA                                
011304                         SEND-KVDLEN                                      
011305                         SEND-DATA                                        
011306******** OM FEL                                                           
011307     IF SEND-KDRC  > 0                                                    
011308        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
011309        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
011310        DELIMITED BY SIZE INTO FELTEXT                                    
011311        DISPLAY FELTEXT                                                   
011312        CALL FELLOG                                                       
011313     END-IF                                                               
011314     .                                                                    
011315     EJECT                                                                
011316 S06-SEND-CLOSE SECTION.                                                  
011317                                                                          
011318       MOVE 'CLOSE' TO SEND-KDFUNC                                        
011319       CALL WZ01SEND USING SEND-CONTROL-AREA                              
011320******** OM FEL                                                           
011321     IF SEND-KDRC  > 0                                                    
011322        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
011323        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
011324        DELIMITED BY SIZE INTO FELTEXT                                    
011325        DISPLAY FELTEXT                                                   
011326        CALL FELLOG                                                       
011327     END-IF                                                               
011330     .                                                                    
011400     EJECT                                                                
011500 X-TAG-CHECKPOINT   SECTION.                                              
011600                                                                          
011700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
011800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
011900     PERFORM IMS-CHECKPOINT                                               
012000     MOVE ZERO TO CHKP-ANT                                                
012100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
012101                                                                          
012102     MOVE PRQ-IDDISTR       TO W-IDDISTR                                  
012124     MOVE PRQ-IDKUNDNR      TO W-IDKUNDNR                                 
012130     MOVE PRQ-IDBUNDLE      TO W-PRQ-IDBUNDLE                             
012150                                                                          
012160     PERFORM IMS-GU-WDC701                                                
012200     .                                                                    
012300     EJECT                                                                
026800* --- IMS SEKTIONER ---                                                   
026900     SKIP3                                                                
027000 IMS-RESTART SECTION.                                                     
027100     SKIP2                                                                
027200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027300     MOVE '  ' TO GODK-STATUSKODER                                        
027400     CALL CBLTDLI USING XRST MSG-PCB                                      
027500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027600                        CHKP-AREA-LENGTH CHKP-AREA                        
027700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     SKIP3                                                                
028100 IMS-CHECKPOINT SECTION.                                                  
028200     SKIP2                                                                
028300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028400     MOVE '  XD' TO GODK-STATUSKODER                                      
028500     CALL CBLTDLI USING CHKP MSG-PCB                                      
028600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028700                        CHKP-AREA-LENGTH CHKP-AREA                        
028800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028900     PERFORM IMS-STATUSKONTROLL                                           
028910                                                                          
028920     IF IMS-EJ-OK                                                         
028930       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT                
028940       DISPLAY FELTEXT                                                    
028941       CALL FELLOG                                                        
028942     END-IF                                                               
028943     .                                                                    
028944     EJECT                                                                
028945 IMS-GU-WDC701 SECTION.                                                   
028946                                                                          
028947     STRING 'WDC701  (WDC701KY =' W-WDC701-X ')'                          
028948          DELIMITED BY SIZE INTO SSA1                                     
028949     MOVE '  GE'         TO GODK-STATUSKODER                              
028950     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
028960     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
028970     PERFORM IMS-STATUSKONTROLL                                           
028971     .                                                                    
028972     SKIP3                                                                
029011 IMS-GHNP-WDC711 SECTION.                                                 
029012                                                                          
029013     STRING 'WDC711  (IDPRQUES =' W-WDC711-X ')'                          
029014          DELIMITED BY SIZE INTO SSA1                                     
029015     MOVE '  GE'         TO GODK-STATUSKODER                              
029016     CALL CBLTDLI USING GHNP WDC7-PCB DLI-IO-WDC711 SSA1                  
029017     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
029018     PERFORM IMS-STATUSKONTROLL                                           
029019     .                                                                    
029020     SKIP3                                                                
029021 IMS-REPL-WDC711 SECTION.                                                 
029022                                                                          
029023     MOVE '  ' TO GODK-STATUSKODER                                        
029024     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
029025     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
029026     PERFORM IMS-STATUSKONTROLL                                           
029027     .                                                                    
029028     SKIP3                                                                
029029 IMS-GU-WDGX3102 SECTION.                                                 
029030                                                                          
029031     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
029032          DELIMITED BY SIZE INTO SSA1                                     
029033     MOVE   'WDGX3102' TO SSA2                                            
029034     MOVE '  ' TO GODK-STATUSKODER                                        
029035     CALL CBLTDLI USING GU WDR4-PCB DLI-IO-WDGX3102                       
029036                                     SSA1 SSA2                            
029037     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
029038     PERFORM IMS-STATUSKONTROLL                                           
029039     .                                                                    
029040     SKIP3                                                                
029100 IMS-STATUSKONTROLL SECTION.                                              
029200                                                                          
029300     SET STATUS-IX TO 1                                                   
029400     SEARCH GODK-STATUS                                                   
029500       AT END                                                             
029600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029700         DELIMITED BY SIZE INTO FELTEXT                                   
029800         CALL FELLOG                                                      
029900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030000         CONTINUE                                                         
030100     END-SEARCH                                                           
030200     .                                                                    
