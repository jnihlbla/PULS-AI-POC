000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0069400.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   JANUARI 1993.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        VCOM DISPATCHER                                                  
000900*        TRANS STARTAS FRÅN VCOM.                                         
001000*        PROGRAMMET HÄMTAR INFO FRÅN VCOM,                                
001100*        FYLLER I MID OCH SÄNDER TILL BERÖRT PROGRAM.                     
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W00694X                                             
001500*        MID:         W0I69401                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         MID TILL MOTTAGANDE PROGRAM                         
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400                                                                          
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                   PIC X(8)    VALUE 'W0069400'.                
002800 77  W-COMPILED              PIC X(16)   VALUE SPACE.                     
002900 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
003000 77  JA                      PIC X       VALUE 'J'.                       
003100 77  NEJ                     PIC X       VALUE 'N'.                       
003200 77  W-IDLTERM               PIC X(8)    VALUE 'R19311  '.                
003300 77  W-CALL-002              PIC X(3)    VALUE '002'.                     
003400 77  W-MOD                   PIC X(8)    VALUE 'W0O50801'.                
003500 77  W-SPAR-1                PIC X(100)  VALUE SPACE.                     
003600 77  W-SPAR-2                PIC X(100)  VALUE SPACE.                     
003700 77  W-POSTANT               PIC S9(3)   VALUE ZERO   COMP-3.             
003800                                                                          
003900 01  W-SENDERTAG.                                                         
004000   03  W-IDCPYTXT            PIC X(8)    VALUE SPACE.                     
004100   03  W-IDSNDJOB            PIC X(8)    VALUE SPACE.                     
004200                                                                          
004300 01  W-VIMSID.                                                            
004400   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004500   03  FILLER                PIC X(4)    VALUE SPACE.                     
004600                                                                          
004700                                                                          
004800 01  GENERELLA-SUBPROGRAM.                                                
004900   03  W006ASCI              PIC X(8)    VALUE 'W006ASCI'.                
005000   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
005100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005300   03  DSCONR                PIC X(8)    VALUE 'DSCONR  '.                
005400   03  DSRECV                PIC X(8)    VALUE 'DSRECV  '.                
005500   03  DSRLSE                PIC X(8)    VALUE 'DSRLSE  '.                
005600                                                                          
005700     EJECT                                                                
005800*- - - - - - - - - - - - - - PARAMETRAR TILL VCOM-AREA                    
005900*01  -COPY W0028 -PRE VCOM-                                               
006000                                                                          
006100     EJECT                                                                
006200******************************************************************        
006300*                                                                         
006400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
006500*                                                                         
006600 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
006700                                                                          
006800 01  MID-W0I69401.                                                        
006900   03  MID-TEST              PIC X(4)    VALUE SPACE.                     
007000   03  MID-DATA              PIC X(28)   VALUE SPACE.                     
007100                                                                          
007200     EJECT                                                                
007300*01  -COPY WMSGSNUF                                                       
007400                                                                          
007500     EJECT                                                                
007600   05  FILLER REDEFINES MSG-INDATA.                                       
007700*    07  -COPY W6I14102 -L                                                
007800                                                                          
007900     EJECT                                                                
008000   05  FILLER REDEFINES MSG-INDATA.                                       
008100*    07  -COPY W6I19C02 -L                                                
008200                                                                          
008300     EJECT                                                                
008400   05  FILLER REDEFINES MSG-INDATA.                                       
008500*    07  -COPY W6I17302 -L                                                
008600                                                                          
008700     EJECT                                                                
008800   03  FILLER REDEFINES MSG-AREA.                                         
008900     05  FILLER              PIC X(84).                                   
009000     05  UTDATA-1            PIC X(100).                                  
009100     05  UTDATA-2            PIC X(100).                                  
009200     05  UTDATA-3            PIC X(100).                                  
009300     05  UTDATA-4            PIC 9(3).                                    
009400     05  UTDATA-5            PIC X(80).                                   
009500                                                                          
009600     EJECT                                                                
009700*01  -COPY WMSGSPAR                                                       
009800                                                                          
009900     EJECT                                                                
010000******************************************************************        
010100*                                                                         
010200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400 01  IMS-WS.                                                              
010500   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
010600                                                                          
010700*                        **** STATUS-KOD FRÅN IMS                         
010800   03  STATUS-WS             PIC XX.                                      
010900     88  STATUS-OK                       VALUE '  '.                      
011000     88  SEGMENT-FINNS                   VALUE '  '.                      
011100                                                                          
011200   03  GODK-STATUSKODER.                                                  
011300     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
011400                                                                          
011500 01    SSA1                  PIC X(32).                                   
011600                                                                          
011700     EJECT                                                                
011800*                            IMS FUNKTIONSKODER                           
011900*01    -COPY W0003                                                        
012000                                                                          
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300*01  -COPY W0009     -PRE MSG-                                            
012400                                                                          
012500*01  -COPY W0009     -PRE ALT-                                            
012600                                                                          
012700     EJECT                                                                
012800*01  -COPY W0009     -PRE ALT6141-                                        
012900                                                                          
013000*01  -COPY W0009     -PRE ALT619C-                                        
013100                                                                          
013200*01  -COPY W0009     -PRE ALT6173-                                        
013300                                                                          
013400     EJECT                                                                
013500 PROCEDURE DIVISION USING MSG-PCB ALT-PCB                                 
013600                          ALT6141-PCB ALT619C-PCB ALT6173-PCB.            
013700 STYR SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
013900                          ALT6141-PCB ALT619C-PCB ALT6173-PCB.            
014000                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT                                                     
014400       IF MID-TEST = 'TEST'                                               
014500         MOVE MID-DATA TO MSG-INDATA                                      
014600       ELSE                                                               
014700         PERFORM B-VCOM-OPEN                                              
014800         IF VCOM-RC = ZERO                                                
014900           PERFORM C-VCOM-READ                                            
015000           PERFORM D-VCOM-CLOSE                                           
015100         END-IF                                                           
015200       END-IF                                                             
015300                                                                          
015400       EVALUATE W-IDCPYTXT                                                
015500         WHEN 'W6I14102' PERFORM IMS-INSERT-6141                          
015600                                                                          
015700         WHEN 'W6I19C02' PERFORM IMS-INSERT-619C                          
015800                                                                          
015900         WHEN 'W6I14101' PERFORM IMS-INSERT-6141                          
016000                                                                          
016100         WHEN 'W6I19C01' PERFORM IMS-INSERT-619C                          
016200                                                                          
016300         WHEN 'W6I17301' PERFORM IMS-INSERT-6173                          
016310*        DISPLAY ' RAHL = ' MSG-INDATA (1:50)                             
016400                                                                          
016500         WHEN OTHER                                                       
016600                         MOVE +475 TO MSG-KVLL                            
016700                         MOVE '0694' TO MSG-AREA                          
016800                         MOVE W-SENDERTAG TO UTDATA-1                     
016900                         MOVE W-SPAR-1    TO UTDATA-2                     
017000                         MOVE W-SPAR-2    TO UTDATA-3                     
017100                         MOVE W-POSTANT   TO UTDATA-4                     
017200                         MOVE FELTEXT     TO UTDATA-5                     
017400                       IF MID-TEST = 'TEST' OR 'VCOM'                     
017500                         PERFORM IMS-INSERT-MSG                           
017600                       ELSE                                               
017700                         PERFORM IMS-CHANGE-ALTMSG                        
017800                         IF STATUS-OK                                     
017900                           PERFORM IMS-INSERT-ALTMSG                      
018000*                        ELSE                                             
018100*                          CALL FELLOG                                    
018200                         END-IF                                           
018300                       END-IF                                             
018400       END-EVALUATE                                                       
018500     END-IF                                                               
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000                                                                          
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019300                                                                          
019400     MOVE WHEN-COMPILED TO W-COMPILED                                     
019500                                                                          
019600     MOVE MSG-INDATA TO MID-W0I69401                                      
019700     MOVE MSG-IDTRANS TO MSG-SPAR-IDTRANS                                 
019800     MOVE MSG-KDMFSFOR TO MSG-SPAR-KDMFSFOR                               
019900     MOVE MSG-KDTRTYP TO MSG-SPAR-KDTRTYP                                 
020000     MOVE SPACE TO MSG-SPAR-IDPFK                                         
020100     .                                                                    
020200                                                                          
020300     EJECT                                                                
020400 B-VCOM-OPEN SECTION.                                                     
020500                                                                          
020600     CALL VIMSID USING W-IMSID                                            
020700     IF W-IMSID = 'IM1P' OR 'IMG0'                                        
020800       MOVE 'W006X1SE' TO VCOM-EXPEDITER                                  
020900     ELSE                                                                 
021000       MOVE 'W006X1TT' TO VCOM-EXPEDITER                                  
021100     END-IF                                                               
021200     CALL DSCONR USING VCOM-RC                                            
021300                       VCOM-DISTID                                        
021400                       VCOM-SECUR                                         
021500                       VCOM-TIMEOUT                                       
021600                       VCOM-SENDERTAG                                     
021700                       VCOM-EXPEDITER                                     
021800                       VCOM-RECTYPE                                       
021900     IF VCOM-RC = +0 OR +41                                               
022000       MOVE VCOM-RC TO W-POSTANT                                          
022100     ELSE                                                                 
022200       MOVE 'DSCONR RC NOT = 0 OR 41' TO FELTEXT                          
022300*      CALL FELLOG                                                        
022400       MOVE ZERO TO VCOM-RC                                               
022500     END-IF                                                               
022600     MOVE VCOM-SENDERTAG TO W-SENDERTAG                                   
022700     .                                                                    
022800                                                                          
022900     EJECT                                                                
023000 C-VCOM-READ SECTION.                                                     
023100                                                                          
023200     MOVE +1000 TO VCOM-MAXLENGTH                                         
023300     CALL DSRECV USING VCOM-RC                                            
023400                       VCOM-DISTID                                        
023500                       VCOM-MAXLENGTH                                     
023600                       VCOM-ACTLENGTH                                     
023700                       VCOM-DATA                                          
023800     IF VCOM-RC NOT = ZERO                                                
023900       MOVE 'DSRECV RC NOT = 0' TO FELTEXT                                
024000       CALL FELLOG                                                        
024100     END-IF                                                               
024200     MOVE VCOM-DATA TO W-SPAR-1                                           
024300                                                                          
024400     CALL W006ASCI USING W-CALL-002                                       
024500                         VCOM-ACTLENGTH                                   
024600                         VCOM-DATA                                        
024700     MOVE VCOM-DATA TO MSG-INDATA                                         
024800                       W-SPAR-2                                           
024900                                                                          
025000     PERFORM UNTIL VCOM-RC NOT = 0                                        
025100       CALL DSRECV USING VCOM-RC                                          
025200                         VCOM-DISTID                                      
025300                         VCOM-MAXLENGTH                                   
025400                         VCOM-ACTLENGTH                                   
025500                         VCOM-DATA                                        
025600       ADD +1 TO W-POSTANT                                                
025700     END-PERFORM                                                          
025800*    IF VCOM-RC NOT = 45                                                  
025900*      MOVE 'DSRECV 2 RC NOT = 45' TO FELTEXT                             
026000*      CALL FELLOG                                                        
026100*    END-IF                                                               
026200     .                                                                    
026300                                                                          
026400     EJECT                                                                
026500 D-VCOM-CLOSE SECTION.                                                    
026600                                                                          
026700     MOVE ZERO TO VCOM-RC                                                 
026800     CALL DSRLSE USING VCOM-RC                                            
026900                       VCOM-DISTID                                        
027000                       VCOM-RVALUE                                        
027100     IF VCOM-RC NOT = ZERO                                                
027200       MOVE 'DSRLSE RC NOT = 0' TO FELTEXT                                
027300       CALL FELLOG                                                        
027400     END-IF                                                               
027500     .                                                                    
027600                                                                          
027700     EJECT                                                                
027800* IMS SEKTIONER                                                           
027900                                                                          
028000 IMS-GET-MSG SECTION.                                                     
028100                                                                          
028200     MOVE '  QC' TO GODK-STATUSKODER                                      
028300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA-SNUF                       
028400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028500     PERFORM IMS-STATUSKONTROLL                                           
028600     .                                                                    
028700                                                                          
028800                                                                          
028900 IMS-INSERT-MSG SECTION.                                                  
029000                                                                          
029100     MOVE SPACE TO GODK-STATUSKODER                                       
029200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA-SNUF W-MOD               
029300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600                                                                          
029700                                                                          
029800 IMS-CHANGE-ALTMSG SECTION.                                               
029900                                                                          
030000     MOVE '  A1A4' TO GODK-STATUSKODER                                    
030100     CALL CBLTDLI USING CHNG ALT-PCB W-IDLTERM                            
030200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500                                                                          
030600                                                                          
030700 IMS-INSERT-ALTMSG SECTION.                                               
030800                                                                          
030900     MOVE SPACE TO GODK-STATUSKODER                                       
031000     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA-SNUF                     
031100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400                                                                          
031500     EJECT                                                                
031600 IMS-INSERT-6141 SECTION.                                                 
031700                                                                          
031800     MOVE +34  TO MSG-KVLL                                                
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE 'W6T141X ' TO MSG-KDTRANS                                       
032100     MOVE '0694' TO MSG-IDTRANS                                           
032200     MOVE '1' TO MSG-KDMFSFOR                                             
032300     MOVE SPACE TO GODK-STATUSKODER                                       
032400     CALL CBLTDLI USING ISRT ALT6141-PCB MSG-IO-AREA-SNUF                 
032500     MOVE ALT6141-STATUS-CODE TO STATUS-WS                                
032600     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800                                                                          
032900                                                                          
033000 IMS-INSERT-619C SECTION.                                                 
033100                                                                          
033200     MOVE +34  TO MSG-KVLL                                                
033300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
033400     MOVE 'W6T19CX ' TO MSG-KDTRANS                                       
033500     MOVE '0694' TO MSG-IDTRANS                                           
033600     MOVE '1' TO MSG-KDMFSFOR                                             
033700     MOVE SPACE TO GODK-STATUSKODER                                       
033800     CALL CBLTDLI USING ISRT ALT619C-PCB MSG-IO-AREA-SNUF                 
033900     MOVE ALT619C-STATUS-CODE TO STATUS-WS                                
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200                                                                          
034300 IMS-INSERT-6173 SECTION.                                                 
034400                                                                          
034500     MOVE +56  TO MSG-KVLL                                                
034600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
034700     MOVE 'W6T173X ' TO MSG-KDTRANS                                       
034800     MOVE '0694' TO MSG-IDTRANS                                           
034900     MOVE '1' TO MSG-KDMFSFOR                                             
035000     MOVE SPACE TO GODK-STATUSKODER                                       
035100     CALL CBLTDLI USING ISRT ALT6173-PCB MSG-IO-AREA-SNUF                 
035200     MOVE ALT6173-STATUS-CODE TO STATUS-WS                                
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500                                                                          
035600     EJECT                                                                
035700 IMS-STATUSKONTROLL SECTION.                                              
035800                                                                          
035900     SET STATUS-IX TO 1                                                   
036000     SEARCH GODK-STATUS                                                   
036100       AT END                                                             
036200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
036300           DELIMITED BY SIZE INTO FELTEXT                                 
036400         CALL FELLOG                                                      
036500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036600         CONTINUE                                                         
036700     END-SEARCH                                                           
036800     .                                                                    
