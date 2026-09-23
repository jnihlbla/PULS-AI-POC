001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W5550300.                                                
001400 AUTHOR.         MARKUS ASPFJÄLL.                                         
001500 DATE-WRITTEN.   98/01/16.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET LÄSER FILEN W55502 OCH RENSAR                         
002100*        BASEN WDL9/WLLOGA                                                
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- INFIL W55502                                               
003610     SELECT W55502                     ASSIGN TO W55503D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W55502                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004210*01  -COPY WDL901      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W5550300'.            
004700 01  CHKP-VAR.                                                            
004800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  W55502-EOF-SW               PIC X       VALUE 'N'.                   
006110     88  END-OF-W55502                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007510     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007902 01  IN-AREA-START               PIC X(24)   VALUE                        
007903                                             'IN-AREA-START'.             
007904     SKIP2                                                                
007905                                                                          
007910*01  AREA -COPY W55502     -PRE IN-                                       
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI            PIC X(16)   VALUE '**KEY**'.             
008501 01  W-WDL901KY-X.                                                        
008520        03 W-IDARTNR         PIC S9(9)                COMP-3.             
008530        03 W-DAREGDAT        PIC  9(8).                                   
008540        03 W-TIKLOCK         PIC S9(9)                COMP-3.             
008550        03 W-IDSEKVNR        PIC S9(3)                COMP-3.             
008560                                                                          
008570                                                                          
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600                                                                          
010701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
010702 01  DLI-IO-WLLOGA01.                                                     
010710*    03  -COPY WDL901                                                     
010800                                                                          
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400                                                                          
011500*01  -COPY W0009   -PRE MSG-                                              
011601     EJECT                                                                
011602*01  -COPY W0008  -PRE LOGA-                                              
011610     05  FILLER                  PIC X.                                   
011900     EJECT                                                                
012001 PROCEDURE DIVISION  USING MSG-PCB LOGA-PCB.                              
012002 MAIN SECTION.                                                            
012010     ENTRY 'DLITCBL' USING MSG-PCB LOGA-PCB.                              
012100                                                                          
012300     SKIP2                                                                
012400     PERFORM A-INIT                                                       
012510     PERFORM S01-LAES-W55502                                              
012600     PERFORM UNTIL END-OF-W55502                                          
012700       IF CHKP-ANT > CHKP-MAX                                             
012800         PERFORM X-TAG-CHECKPOINT                                         
012900       END-IF                                                             
013000       PERFORM B-BEARBETNING                                              
013610       PERFORM S01-LAES-W55502                                            
013700     END-PERFORM                                                          
013800                                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700     SKIP2                                                                
014900     PERFORM IMS-RESTART                                                  
015110     OPEN INPUT W55502                                                    
015810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016100     .                                                                    
016300     EJECT                                                                
016310 B-BEARBETNING SECTION.                                                   
016320     MOVE IN-RENS-IDARTNR         TO W-IDARTNR                            
016321     MOVE IN-RENS-DAREGDAT-9KOMPL TO W-DAREGDAT                           
016322     MOVE IN-RENS-TIKLOCK-9KOMPL  TO W-TIKLOCK                            
016323     MOVE IN-RENS-IDSEKVNR        TO W-IDSEKVNR                           
016330     PERFORM IMS-GET-LOGA                                                 
016340     IF SEGMENT-FINNS                                                     
016350        PERFORM IMS-DLET-LOGA                                             
016360     END-IF                                                               
016370     ADD +1 TO CHKP-ANT                                                   
016380     .                                                                    
016392     EJECT                                                                
016400 Z-FINIT SECTION.                                                         
016500                                                                          
016901                                                                          
016910     CLOSE W55502                                                         
017101     SKIP2                                                                
017102     MOVE 'S' TO POSTSUM-OPKOD                                            
017110     CALL POSTSUM USING POSTSUM-PARM                                      
017300     .                                                                    
017401     EJECT                                                                
017402 S01-LAES-W55502  SECTION.                                                
017403     SKIP2                                                                
017404     READ W55502 INTO IN-AREA                                             
017405     AT END                                                               
017406*       MOVE HIGH-VALUE TO IN-ID                                          
017407        SET END-OF-W55502 TO TRUE                                         
017408                                                                          
017409     NOT AT END                                                           
017410        MOVE 'W55502' TO POSTSUM-FDNAMN                                   
017411        MOVE 'W55503D1' TO POSTSUM-DDNAMN2                                
017412                                                                          
017413        CALL POSTSUM USING POSTSUM-PARM                                   
017414                                                                          
017415*       ADD 1 TO W-W55502-KVPOST-IN                                       
017416     END-READ                                                             
017420     .                                                                    
017700     EJECT                                                                
017800 X-TAG-CHECKPOINT   SECTION.                                              
017900                                                                          
018000* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
018100* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
018500     PERFORM IMS-CHECKPOINT                                               
018600     MOVE ZERO TO CHKP-ANT                                                
018700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
018800     .                                                                    
018900     EJECT                                                                
019000* --- IMS SEKTIONER ---                                                   
019100     SKIP3                                                                
019201     EJECT                                                                
019202 IMS-GET-LOGA SECTION.                                                    
019203                                                                          
019204     STRING 'WLLOGA01(WDL901KY =' W-WDL901KY-X ')'                        
019205          DELIMITED BY SIZE INTO SSA1                                     
019206     MOVE '  GE' TO GODK-STATUSKODER                                      
019207     CALL CBLTDLI USING GHU LOGA-PCB DLI-IO-WLLOGA01 SSA1                 
019208     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
019209     PERFORM IMS-STATUSKONTROLL                                           
019210     .                                                                    
019211     SKIP3                                                                
019212 IMS-DLET-LOGA SECTION.                                                   
019213                                                                          
019214     MOVE '   ' TO GODK-STATUSKODER                                       
019215     CALL CBLTDLI USING DLET LOGA-PCB DLI-IO-WLLOGA01                     
019216     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
019217     PERFORM IMS-STATUSKONTROLL                                           
019220     .                                                                    
019300     EJECT                                                                
019400 IMS-RESTART SECTION.                                                     
019500     SKIP2                                                                
019600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019700     MOVE '  ' TO GODK-STATUSKODER                                        
019800     CALL CBLTDLI USING XRST MSG-PCB                                      
019900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020000                        CHKP-AREA-LENGTH CHKP-AREA                        
020100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400     EJECT                                                                
020500 IMS-CHECKPOINT SECTION.                                                  
020600     SKIP2                                                                
020700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020800     MOVE '  XD' TO GODK-STATUSKODER                                      
020900     CALL CBLTDLI USING CHKP MSG-PCB                                      
021000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021100                        CHKP-AREA-LENGTH CHKP-AREA                        
021200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021300     PERFORM IMS-STATUSKONTROLL                                           
021400                                                                          
021500     IF IMS-EJ-OK                                                         
021600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021700       DISPLAY FELTEXT                                                    
021800       CALL FELLOG                                                        
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 IMS-STATUSKONTROLL SECTION.                                              
022300     SKIP2                                                                
022400     SET STATUS-IX TO 1                                                   
022500     SEARCH GODK-STATUS                                                   
022600       AT END                                                             
022700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022800           DELIMITED BY SIZE INTO FELTEXT                                 
022900         DISPLAY FELTEXT                                                  
023000         CALL FELLOG                                                      
023100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023200         CONTINUE                                                         
023300     END-SEARCH                                                           
023400     .                                                                    
