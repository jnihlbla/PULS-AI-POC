000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2343000.                                                
000400*AUTHOR.         HENRIK ARONSSON.                                         
000500*DATE-WRITTEN.   APRIL 93.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER HÄNDELESEREGISTER WLXXCZ OCH TAR BORT                      
001100*        SAMTLIGA 'BARN' TILL HTYP 2245 (GODKÄND                          
001300*        LEVERANSPLAN PÅ ARTIKEL MED LV-LEVERANTÖRNR).                    
001310*                                                                         
001400*        PROGRAMMET UPPDATERAR WLXXCZ (WDR5) (HTYP 2245)                  
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2343000'.            
004500 01  CHKP-VAR.                                                            
004600 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004700 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004800 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005100 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400     SKIP2                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL DATKORT                                          
007300*                                                                         
007400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23430'.              
007500     SKIP2                                                                
007600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007700     SKIP2                                                                
007800*01  -COPY WDATKORT                                                       
007900     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009310                                                                          
009400     03  W-WDGXKEY-2245-X.                                                
009410         05  W-IDHTYP-2245       PIC X(4)    VALUE '2245'.                
009420         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009430                                                                          
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                       VALUE '  '.                  
010900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011200     88  IMS-EJ-OK                           VALUE 'XD'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500     SKIP3                                                                
012600 01  DLI-IO-AREA.                                                         
012700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012800     SKIP3                                                                
013200     03  WLXXCZ11 REDEFINES IO-AREA.                                      
013300*        05  -COPY WDGX2246 -PRE XXCZ-                                    
013400     SKIP3                                                                
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400 01  FILLER                      PIC X(16)                                
014500                             VALUE 'DLI-IO-AREA-2'.                       
014600     SKIP3                                                                
014700 01  DLI-IO-AREA-2.                                                       
014800     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
014900     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100                                                                          
015200*01  -COPY W0009   -PRE MSG-                                              
015300     EJECT                                                                
015400*01  -COPY W0008  -PRE XXCZ-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
016000 PROCEDURE DIVISION  USING MSG-PCB XXCZ-PCB.                              
016100     ENTRY 'DLITCBL' USING MSG-PCB XXCZ-PCB.                              
016200                                                                          
016300     SKIP2                                                                
016400     PERFORM A-INIT                                                       
016410     PERFORM IMS-GU-XXCZ01                                                
016420     PERFORM IMS-GHNP-XXCZ11                                              
016430                                                                          
016500     PERFORM UNTIL SEGMENT-SAKNAS                                         
016900                                                                          
017000       PERFORM IMS-DLET-XXCZ                                              
017001       ADD +1 TO CHKP-ANT                                                 
017002                                                                          
017310       IF CHKP-ANT > CHKP-MAX                                             
017320         PERFORM X-TAG-CHECKPOINT                                         
017330       END-IF                                                             
017400                                                                          
017410       PERFORM IMS-GHNP-XXCZ11                                            
017420                                                                          
017500     END-PERFORM                                                          
017700                                                                          
018000     MOVE ZERO TO RETURN-CODE                                             
018100     GOBACK                                                               
018200     .                                                                    
018300     EJECT                                                                
018400 A-INIT SECTION.                                                          
018500     SKIP2                                                                
018600                                                                          
018700     PERFORM IMS-RESTART                                                  
019800     .                                                                    
019900     EJECT                                                                
021900 X-TAG-CHECKPOINT   SECTION.                                              
022000                                                                          
022100* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022200* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
022210                                                                          
022300     PERFORM IMS-CHECKPOINT                                               
022400     MOVE ZERO TO CHKP-ANT                                                
022410                                                                          
022500* --- LÄS OM DATABAS OM DET BEHÖVS                                        
022501                                                                          
022510     PERFORM IMS-GU-XXCZ01                                                
022600     .                                                                    
022700     EJECT                                                                
022800* --- IMS SEKTIONER ---                                                   
022900     SKIP3                                                                
023000     EJECT                                                                
023100 IMS-GU-XXCZ01 SECTION.                                                   
023200                                                                          
023300     STRING 'WLXXCZ01(WDGXKEY  =' W-WDGXKEY-2245-X ')'                    
023400          DELIMITED BY SIZE INTO SSA1                                     
023500     MOVE '  ' TO GODK-STATUSKODER                                        
023600     CALL CBLTDLI USING GU XXCZ-PCB DLI-IO-AREA SSA1                      
023700     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
023800     PERFORM IMS-STATUSKONTROLL                                           
023900     .                                                                    
024000     SKIP3                                                                
024100 IMS-GHNP-XXCZ11 SECTION.                                                 
024200                                                                          
024300     MOVE 'WLXXCZ11 ' TO SSA1                                             
024500     MOVE '  GE' TO GODK-STATUSKODER                                      
024600     CALL CBLTDLI USING GHNP XXCZ-PCB DLI-IO-AREA SSA1                    
024700     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-DLET-XXCZ SECTION.                                                   
025200                                                                          
025400     MOVE '  ' TO GODK-STATUSKODER                                        
025500     CALL CBLTDLI USING DLET XXCZ-PCB DLI-IO-AREA                         
025600     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
025700     PERFORM IMS-STATUSKONTROLL                                           
025800     .                                                                    
025900     EJECT                                                                
029200 IMS-RESTART SECTION.                                                     
029300                                                                          
029400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029500     MOVE '  ' TO GODK-STATUSKODER                                        
029600     CALL CBLTDLI USING XRST MSG-PCB                                      
029700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029800                        CHKP-AREA-LENGTH CHKP-AREA                        
029900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     EJECT                                                                
030300 IMS-CHECKPOINT SECTION.                                                  
030400                                                                          
030500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030600     MOVE '  XD' TO GODK-STATUSKODER                                      
030700     CALL CBLTDLI USING CHKP MSG-PCB                                      
030800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030900                        CHKP-AREA-LENGTH CHKP-AREA                        
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200                                                                          
031300     IF IMS-EJ-OK                                                         
031400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
031500       DISPLAY FELTEXT                                                    
031600       CALL FELLOG                                                        
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-STATUSKONTROLL SECTION.                                              
032100     SKIP2                                                                
032200     SET STATUS-IX TO 1                                                   
032300     SEARCH GODK-STATUS                                                   
032400       AT END                                                             
032500         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
032600           DELIMITED BY SIZE INTO FELTEXT-STR                             
032700         DISPLAY FELTEXT                                                  
032800         CALL FELLOG                                                      
032900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033000         CONTINUE                                                         
033100     END-SEARCH                                                           
033200     .                                                                    
