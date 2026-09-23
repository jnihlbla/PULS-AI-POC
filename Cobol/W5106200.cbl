001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5106200.                                                
001300 AUTHOR.         JONNY SANDSTEN.                                          
001400 DATE-WRITTEN.   98/05/26.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        LÄSER TRANSFIL OCH RENSAR BORT MOTSVARANDE                       
002000*        POSTER PÅ EK.HÄNDELSEBAS(WDR9)                                   
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- INFIL FRÅN PGM W5106100 OCH W5106600                       
003110     SELECT W51061                     ASSIGN TO W51062D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W51061                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY WDR901      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004001                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W5106200'.            
004110 01  VARIABLER.                                                           
004120     03 KVANT-DEL                PIC S9(7)   VALUE +0   COMP-3.           
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W51061-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W51061                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                             'IN-AREA-START'.             
007410*01  AREA -COPY WDR901     -PRE IN-                                       
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-WDR901KY-X.                                                    
008010         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
008020         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
008030         05  W-TIKLOCK           PIC S9(9)   COMP-3.                      
008040         05  W-IDSEKVNR          PIC S9(3)   COMP-3.                      
008050         05  W-IDCPYTEXT         PIC X(8).                                
008051         05  FILLER REDEFINES W-IDCPYTEXT.                                
008060             07  W-IDSYSTEM      PIC X(4).                                
008061             07  W-IDPTYP        PIC X(3).                                
008070             07  W-IDVTYP        PIC X.                                   
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                      PIC XX.                               
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA01'.                    
010202 01  DLI-IO-WLSAPA01.                                                     
010210*    03  -COPY WDR901                                                     
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008  -PRE SAPA-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB SAPA-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB SAPA-PCB.                              
011600                                                                          
011900     PERFORM A-INIT                                                       
012000                                                                          
012010     PERFORM S01-LAES-W51061                                              
012100     PERFORM UNTIL END-OF-W51061                                          
012200       IF CHKP-ANT > CHKP-MAX                                             
012300         PERFORM X-TAG-CHECKPOINT                                         
012400       END-IF                                                             
012510       PERFORM B-BEARBETNING                                              
013110       PERFORM S01-LAES-W51061                                            
013200     END-PERFORM                                                          
013300                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014300                                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W51061                                                    
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-BEARBETNING SECTION.                                                   
015811                                                                          
015820     MOVE IN-FIL-IDPGM       TO W-IDPGM                                   
015830     MOVE IN-FIL-DAREGDAT    TO W-DAREGDAT                                
015840     MOVE IN-FIL-TIKLOCK     TO W-TIKLOCK                                 
015850     MOVE IN-FIL-IDSEKVNR    TO W-IDSEKVNR                                
015851     MOVE IN-FIL-CT-IDSYSTEM TO W-IDSYSTEM                                
015852     MOVE IN-FIL-CT-IDPTYP   TO W-IDPTYP                                  
015853     MOVE IN-FIL-CT-IDVTYP   TO W-IDVTYP                                  
015860     PERFORM IMS-GET-SAPA                                                 
015870     IF SEGMENT-FINNS                                                     
015880       PERFORM IMS-DLET-SAPA                                              
015881       ADD +1                TO KVANT-DEL                                 
015890     END-IF                                                               
015891     ADD +1 TO CHKP-ANT                                                   
015892     .                                                                    
015893     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016401                                                                          
016410     CLOSE W51061                                                         
016601                                                                          
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016620     DISPLAY ' BORTTAGNA SEGMENT '  KVANT-DEL                             
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-W51061  SECTION.                                                
016903     SKIP2                                                                
016904     READ W51061 INTO IN-AREA                                             
016905     AT END                                                               
016907        SET END-OF-W51061 TO TRUE                                         
016909     NOT AT END                                                           
016910        MOVE 'W51061'   TO POSTSUM-FDNAMN                                 
016911        MOVE 'W51062D1' TO POSTSUM-DDNAMN2                                
016912        MOVE 'RENS'     TO POSTSUM-TRANSTYP                               
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016916     END-READ                                                             
016920     .                                                                    
017298     EJECT                                                                
017300 X-TAG-CHECKPOINT   SECTION.                                              
017400                                                                          
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018300     .                                                                    
018400                                                                          
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018701     EJECT                                                                
018702 IMS-GET-SAPA SECTION.                                                    
018703                                                                          
018704     STRING 'WLSAPA01(WDR901KY =' W-WDR901KY-X ')'                        
018705          DELIMITED BY SIZE INTO SSA1                                     
018706     MOVE '  GE' TO GODK-STATUSKODER                                      
018707     CALL CBLTDLI USING GHU SAPA-PCB DLI-IO-WLSAPA01 SSA1                 
018708     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
018709     PERFORM IMS-STATUSKONTROLL                                           
018710     .                                                                    
018711     SKIP3                                                                
018712 IMS-DLET-SAPA SECTION.                                                   
018713                                                                          
018714     MOVE '  ' TO GODK-STATUSKODER                                        
018715     CALL CBLTDLI USING DLET SAPA-PCB DLI-IO-WLSAPA01                     
018716     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
018717     PERFORM IMS-STATUSKONTROLL                                           
018720     .                                                                    
018800     EJECT                                                                
018900 IMS-RESTART SECTION.                                                     
019000     SKIP2                                                                
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GODK-STATUSKODER                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSKONTROLL                                           
019800     .                                                                    
019900     SKIP3                                                                
020000 IMS-CHECKPOINT SECTION.                                                  
020100     SKIP2                                                                
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900                                                                          
021000     IF IMS-EJ-OK                                                         
021100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200       DISPLAY FELTEXT                                                    
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 IMS-STATUSKONTROLL SECTION.                                              
021800     SKIP2                                                                
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
