000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3352100.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   93/10/14.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        BETALAREINFORMATION                                              
001100*        RENSAR KUNDREGISTRET FRÅN INAKTUELLA                             
001400*        KAMPANJRABATTER.                                                 
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLPRIB (WDC2)                              
001700*                                                                         
002200                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900                                                                          
003000 DATA DIVISION.                                                           
003100                                                                          
003200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004310*    -COPY WY2000W1                                                       
004400     SKIP3                                                                
004401 01  CHKP-VAR.                                                            
004402 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004403 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004404 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004405 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004406 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004410 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004500 77  IDPGM                       PIC X(8)    VALUE 'W3352100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC 9(2)    COMP SYNC.                   
004900 77  SPAR-IDMARKBO               PIC X(1)    VALUE SPACE.                 
005200 77  SPAR-IDPROMR                PIC X(3)    VALUE SPACE.                 
005300     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900                                                                          
006200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006300                                                                          
006400 77  NY-TABELL-SW                PIC X       VALUE 'N'.                   
006500     88  NY-TABELL                           VALUE 'J'.                   
006600     EJECT                                                                
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009400     03  W-IDPROMR-X.                                                     
009500         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
009800     03  W-WDC212KY-X.                                                    
009900         05  W-KDARTKAM          PIC 9(5)   VALUE ZERO.                   
009910         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
010000     SKIP2                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010700     88  IMS-EJ-OK                           VALUE 'XD'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012000     SKIP3                                                                
012100 01  DLI-IO-AREA.                                                         
012200     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
012300     SKIP3                                                                
012400     03  WLPRIB01 REDEFINES IO-AREA.                                      
012500*        05  -COPY WDC201  -PRE PRIB-                                     
012600     SKIP3                                                                
012700     03  WLPRIB11 REDEFINES IO-AREA.                                      
012800*        05  -COPY WDC212  -PRE KAMP-                                     
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013200*01  -COPY W0009   -PRE MSG-                                              
013300     EJECT                                                                
013400*01  -COPY W0008  -PRE PRIB-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700 PROCEDURE DIVISION  USING MSG-PCB  PRIB-PCB.                             
013710 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB  PRIB-PCB.                             
013900                                                                          
014100     PERFORM A-INIT                                                       
014900                                                                          
014910     PERFORM B-RENSA-PRIB-KAMP                                            
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015800                                                                          
015810     PERFORM IMS-RESTART                                                  
015900     ACCEPT DAGENS-DATUM FROM DATE                                        
016200                                                                          
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     .                                                                    
016500     EJECT                                                                
019400 B-RENSA-PRIB-KAMP SECTION.                                               
019500                                                                          
019600     PERFORM IMS-GN-PRIB-PRIB                                             
019700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
019800       MOVE PRIB-PRO-IDPROMR TO SPAR-IDPROMR                              
020200*                            LÄS KAMPSEGMENT SPARA DATUM(NYCKELN)         
020410       PERFORM IMS-GHNP-KAMP                                              
020420       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
020430         IF CHKP-ANT > CHKP-MAX                                           
020440           PERFORM X-TAG-CHECKPOINT                                       
020441           PERFORM IMS-GU-PRIB                                            
020442           PERFORM IMS-GHNP-KAMP                                          
020450         END-IF                                                           
020451         MOVE KAMP-KAM-TISTODAT   TO TMP1-YYMMDD                          
020452         MOVE DAGENS-DATUM        TO TMP2-YYMMDD                          
020460         PERFORM WY2000P1                                                 
020600         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
020900*                                          TAG BORT KAMP SEGMENT          
021000           PERFORM IMS-DLET-PRIB                                          
021100           ADD +1 TO CHKP-ANT                                             
021801         END-IF                                                           
021802*                                          LÄS NÄSTA KAMP SEGMENT         
021803         PERFORM IMS-GHNP-KAMP                                            
021804       END-PERFORM                                                        
021810       PERFORM IMS-GN-PRIB-PRIB                                           
021900     END-PERFORM                                                          
022200     .                                                                    
022300     EJECT                                                                
022301 X-TAG-CHECKPOINT   SECTION.                                              
022302                                                                          
022306     PERFORM IMS-CHECKPOINT                                               
022307     MOVE ZERO TO CHKP-ANT                                                
022309     MOVE SPAR-IDPROMR TO W-IDPROMR                                       
022310     .                                                                    
022400     EJECT                                                                
026200* --- IMS SEKTIONER ---                                                   
026300                                                                          
026500 IMS-GN-PRIB-PRIB SECTION.                                                
026600     MOVE 'WLPRIB01 ' TO SSA1                                             
026700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
026800     CALL CBLTDLI USING GN PRIB-PCB DLI-IO-AREA SSA1                      
026900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     SKIP3                                                                
027300 IMS-GU-PRIB SECTION.                                                     
027400     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
027410     DELIMITED BY SIZE INTO SSA1                                          
027500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
027600     CALL CBLTDLI USING GU PRIB-PCB DLI-IO-AREA SSA1                      
027700     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     SKIP3                                                                
028210 IMS-GHNP-KAMP SECTION.                                                   
028600     MOVE 'WLPRIB12 '         TO SSA1                                     
028700     MOVE '  GE' TO GODK-STATUSKODER                                      
028800     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-AREA SSA1                    
028900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     SKIP3                                                                
031200 IMS-DLET-PRIB SECTION.                                                   
031300                                                                          
031400     MOVE '  ' TO GODK-STATUSKODER                                        
031500     CALL CBLTDLI USING DLET PRIB-PCB DLI-IO-AREA                         
031600     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
031929 IMS-RESTART SECTION.                                                     
031930                                                                          
031931     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031932     MOVE '  ' TO GODK-STATUSKODER                                        
031933     CALL CBLTDLI USING XRST MSG-PCB                                      
031934                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031935                        CHKP-AREA-LENGTH CHKP-AREA                        
031936     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031937     PERFORM IMS-STATUSKONTROLL                                           
031938     .                                                                    
031939     SKIP3                                                                
031940 IMS-CHECKPOINT SECTION.                                                  
031941                                                                          
031942     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031943     MOVE '  XD' TO GODK-STATUSKODER                                      
031944     CALL CBLTDLI USING CHKP MSG-PCB                                      
031945                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031946                        CHKP-AREA-LENGTH CHKP-AREA                        
031947     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031948     PERFORM IMS-STATUSKONTROLL                                           
031949                                                                          
031950     IF IMS-EJ-OK                                                         
031951       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
031952       DISPLAY FELTEXT                                                    
031953       CALL FELLOG                                                        
031954     END-IF                                                               
031955     .                                                                    
031960     EJECT                                                                
032000 IMS-STATUSKONTROLL SECTION.                                              
032100                                                                          
032200     SET STATUS-IX TO 1                                                   
032300     SEARCH GODK-STATUS                                                   
032400       AT END                                                             
032500         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
032600         DISPLAY FELTEXT                                                  
032700         CALL FELLOG                                                      
032800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032900         CONTINUE                                                         
033000     END-SEARCH                                                           
033100     .                                                                    
033110     EJECT                                                                
033200*    -COPY WY2000P1                                                       
