000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W006PRC1.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   JAN.  93.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        GENERELLT LISTNINGSPROGRAM                                       
001000*        VIA VCOM                                                         
001100*        ELLER                                                            
001200*        VIA SPOOL-API (ENDAST TEST)                                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001600*                                                                         
001700*    UTDATA.                                                              
001800*        DATA TILL VCOM                                                   
001900*        ELLER                                                            
002000*        RADER TILL SPOOL-API                                             
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                   PIC X(8)    VALUE 'W006PRC1'.                
003000 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
003100 77  JA                      PIC X       VALUE 'J'.                       
003200 77  NEJ                     PIC X       VALUE 'N'.                       
003300 77  W-PRTTYP                PIC X(5)    VALUE SPACE.                     
003400 77  W-IDPRTLST              PIC X(8)    VALUE HIGH-VALUE.                
003500 77  W-CALL-001              PIC X(3)    VALUE '001'.                     
003600 77  W-RADANT                PIC S9(7)   VALUE ZERO  COMP-3.              
003700 77  W-RC-DISPL              PIC 9(4)    VALUE ZERO.                      
003800 77  VCOM-IX                 PIC S9(1)   COMP-3.                          
003900 77  FEM-SEKUNDER            PIC S9(9)   VALUE +500 COMP.                 
004000                                                                          
004100 01  W-VIMSID.                                                            
004200   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004300   03  FILLER                PIC X(4)    VALUE SPACE.                     
004400                                                                          
004500 01  W-LIST-TYP.                                                          
004600   03  W-IDVCOM.                                                          
004700     05  W-IDVCOM-1-4        PIC X(4)    VALUE SPACE.                     
004800     05  FILLER              PIC X(2)    VALUE SPACE.                     
004900     05  W-IDVCOM-7-8        PIC X(2)    VALUE SPACE.                     
005000   03  W-IDCPYTXT            PIC X(8)    VALUE SPACE.                     
005100                                                                          
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400   03  W006ASCI              PIC X(8)    VALUE 'W006ASCI'.                
005500   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
005600   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
005700   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005800   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005900   03  DSCONS                PIC X(8)    VALUE 'DSCONS  '.                
006000   03  DSSEND                PIC X(8)    VALUE 'DSSEND  '.                
006100   03  DSRLSE                PIC X(8)    VALUE 'DSRLSE  '.                
006200   03  W009WAIT              PIC X(8)    VALUE 'W009WAIT'.                
006300     EJECT                                                                
006400 01  FILLER                  PIC X(16)   VALUE 'SPOOL-AREA'.              
006500*01  -COPY WMSGSPOL                                                       
006600                                                                          
006700     EJECT                                                                
006800*- - - - - - - - - - - - - - PARAMETRAR TILL W006PRT                      
006900 01  FILLER                  PIC X(16)   VALUE 'W006PRT   '.              
007000*01  -COPY W006PRT.                                                       
007100     EJECT                                                                
007200 01  FILLER                  PIC X(16)   VALUE 'W006PRAR  '.              
007300*01  -COPY W006PRAR.                                                      
007400     EJECT                                                                
007500*- - - - - - - - - - - - - - PARAMETRAR TILL VCOM                         
007600 01  FILLER                  PIC X(16)   VALUE 'W0028     '.              
007700*01  -COPY W0028  -PRE VCOM-                                              
007800     EJECT                                                                
007900******************************************************************        
008000*                                                                         
008100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008200*                                                                         
008300 01  IMS-WS.                                                              
008400   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
008500     SKIP3                                                                
008600*                        **** STATUS-KOD FRÅN IMS                         
008700   03  STATUS-WS             PIC XX.                                      
008800     88  SEGMENT-FINNS                   VALUE '  '.                      
008900     88  INSERTEN-OK                     VALUE '  '.                      
009000                                                                          
009100   03  GODK-STATUSKODER.                                                  
009200     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
009300     EJECT                                                                
009400*                            IMS FUNKTIONSKODER                           
009500*01    -COPY W0003                                                        
009600     EJECT                                                                
009700 LINKAGE SECTION.                                                         
009800                                                                          
009900 01  LINK-LIST-TYP           PIC X(16).                                   
010000                                                                          
010100                                                                          
010200 01  LINK-CALL-TYP           PIC X(5).                                    
010300                                                                          
010400                                                                          
010500 01  LINK-IDPRTLST           PIC X(8).                                    
010600                                                                          
010700                                                                          
010800*01  -COPY W0009 -PRE ALT-.                                               
010900                                                                          
011000     EJECT                                                                
011100                                                                          
011200*01  -COPY W006PRVC.                                                      
011300                                                                          
011400     EJECT                                                                
011500 PROCEDURE DIVISION USING  LINK-LIST-TYP                                  
011600                           LINK-CALL-TYP                                  
011700                           LINK-IDPRTLST                                  
011800                           ALT-PCB                                        
011900                           PRC1-W006PRVC.                                 
012000 STYR SECTION.                                                            
012100                                                                          
012200     EVALUATE LINK-CALL-TYP                                               
012300       WHEN PRT-OPEN  PERFORM A-OPEN                                      
012400       WHEN PRT-WRITE PERFORM B-WRITE                                     
012500       WHEN PRT-PURGE PERFORM C-PURGE                                     
012600       WHEN PRT-CLOSE PERFORM D-CLOSE                                     
012700     END-EVALUATE                                                         
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200                                                                          
013300     EJECT                                                                
013400 A-OPEN SECTION.                                                          
013500                                                                          
013600     MOVE LINK-LIST-TYP TO W-LIST-TYP                                     
013700     MOVE LINK-IDPRTLST TO PRT-IDPRTLST                                   
013800                           W-IDPRTLST                                     
013900     MOVE '005' TO PRT-KDCALL                                             
014000     CALL W006PRT USING PRT-W006PRT                                       
014100     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
014200     CALL VIMSID USING W-IMSID                                            
014300     IF W-IMSID = 'IMG0'                                                  
014400       IF W-PRTTYP = 'VCOM ' OR 'VCOMP' OR 'VCOMT'                        
014500         MOVE 'VCOMP' TO W-PRTTYP                                         
014600       END-IF                                                             
014700     ELSE                                                                 
014800       IF W-PRTTYP = 'VCOMT'                                              
014900         IF W-IMSID = 'IMD0'                                              
015000           MOVE 'XX' TO W-IDVCOM-7-8                                      
015100         ELSE                                                             
015210           MOVE 'TT' TO W-IDVCOM-7-8                                      
015310         END-IF                                                           
015400       END-IF                                                             
015500     END-IF                                                               
015600     IF W-PRTTYP = 'VCOMT' OR 'VCOMP'                                     
015700       PERFORM AA-VCOM-START                                              
015800     ELSE                                                                 
015900       MOVE SPACE TO SPOOL-FORMDEF SPOOL-PAGEDEF                          
016000       MOVE 'STD ' TO SPOOL-FORMS                                         
016100       MOVE +104 TO SPOOL-OPT-LL                                          
016200       MOVE +86  TO SPOOL-IAFP-LL                                         
016300       MOVE 'IAFP=A00,PRTO=' TO SPOOL-IAFP                                
016400       MOVE 'H' TO SPOOL-CLASS                                            
016500       MOVE ',OUTDISP(HOLD,HOLD)' TO SPOOL-OVR-PARAM                      
016600       IF PRC1-KVLRECL > 120                                              
016700         MOVE 120          TO SPOOL-RAD-BDW                               
016800                              SPOOL-RAD-RDW (1)                           
016900       ELSE                                                               
017000         MOVE PRC1-KVLRECL TO SPOOL-RAD-BDW                               
017100                              SPOOL-RAD-RDW (1)                           
017200       END-IF                                                             
017300       ADD +9 TO SPOOL-RAD-BDW                                            
017400       ADD +5 TO SPOOL-RAD-RDW (1)                                        
017500       MOVE ZERO TO SPOOL-RAD-ZZ  (1)                                     
017600       MOVE SPACE TO SPOOL-RAD    (1)                                     
017700       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
017800       MOVE PRT-IDLTERM TO SPOOL-IDNODE                                   
017900       PERFORM IMS-CHANGE-SPOOL                                           
018000     END-IF                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 AA-VCOM-START SECTION.                                                   
018400                                                                          
018500     IF PRC1-IDVCINIT = 'W006ASCI' OR SPACE                               
018600       MOVE SPACE         TO VCOM-INITIATOR                               
018700     ELSE                                                                 
018800       MOVE PRC1-IDVCINIT TO VCOM-INITIATOR                               
018900     END-IF                                                               
019000     IF PRC1-TEVCOMST = 'W006PRT '                                        
019100       MOVE PRT-IDNODE TO VCOM-SENDERTAG                                  
019200     ELSE                                                                 
019300       MOVE PRC1-TEVCOMST TO VCOM-SENDERTAG                               
019400     END-IF                                                               
019500     MOVE W-IDVCOM  TO VCOM-PARTNER                                       
019600     MOVE 'N'       TO VCOM-PRIO                                          
019700     MOVE 1 TO VCOM-IX                                                    
019800     CALL DSCONS USING VCOM-RC                                            
019900                       VCOM-DISTID                                        
020000                       VCOM-SECUR                                         
020100                       VCOM-TIMEOUT                                       
020200                       VCOM-SENDERTAG                                     
020300                       VCOM-PARTNER                                       
020400                       VCOM-RECEIPT                                       
020500                       VCOM-PRIO                                          
020600                       VCOM-INITIATOR                                     
020700     PERFORM UNTIL VCOM-RC = ZERO OR VCOM-IX >= 5                         
020800       ADD 1 TO VCOM-IX                                                   
020900       CALL W009WAIT USING  FEM-SEKUNDER                                  
021000       CALL DSCONS USING VCOM-RC                                          
021100                         VCOM-DISTID                                      
021200                         VCOM-SECUR                                       
021300                         VCOM-TIMEOUT                                     
021400                         VCOM-SENDERTAG                                   
021500                         VCOM-PARTNER                                     
021600                         VCOM-RECEIPT                                     
021700                         VCOM-PRIO                                        
021800                         VCOM-INITIATOR                                   
021900     END-PERFORM                                                          
022000     IF VCOM-RC NOT = ZERO                                                
022100       MOVE VCOM-RC TO W-RC-DISPL                                         
022200       STRING 'FEL FRÅN DSCONS I AA-VCOM-START. RC=' W-RC-DISPL           
022300       DELIMITED BY SIZE INTO FELTEXT                                     
022400       CALL FELLOG                                                        
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 B-WRITE SECTION.                                                         
022900                                                                          
023000     IF W-PRTTYP = 'VCOMT' OR 'VCOMP'                                     
023100       MOVE PRC1-KVLRECL TO VCOM-ACTLENGTH                                
023200       MOVE PRC1-DATA TO VCOM-DATA                                        
023300       IF PRC1-IDVCINIT = 'W006ASCI'                                      
023400         CALL W006ASCI USING W-CALL-001 VCOM-ACTLENGTH VCOM-DATA          
023500       END-IF                                                             
023600                                                                          
023700       CALL DSSEND USING VCOM-RC                                          
023800                         VCOM-DISTID                                      
023900                         VCOM-ACTLENGTH                                   
024000                         VCOM-DATA                                        
024100       IF VCOM-RC NOT = ZERO                                              
024200         MOVE VCOM-RC TO W-RC-DISPL                                       
024300         STRING 'FEL FRÅN DSSEND I B-WRITE. RC=' W-RC-DISPL               
024400         DELIMITED BY SIZE INTO FELTEXT                                   
024500         CALL FELLOG                                                      
024600       END-IF                                                             
024700     ELSE                                                                 
024800       MOVE PRC1-DATA TO SPOOL-DATA                                       
024900       PERFORM IMS-INSERT-SPOOL                                           
025000       MOVE ' ' TO SPOOL-RAD-STYR (1)                                     
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 C-PURGE SECTION.                                                         
025500                                                                          
025600     PERFORM IMS-PURGE-SPOOL                                              
025700     .                                                                    
025800     EJECT                                                                
025900 D-CLOSE SECTION.                                                         
026000                                                                          
026100     IF W-PRTTYP = 'VCOMT' OR 'VCOMP'                                     
026200       MOVE +0 TO VCOM-RC                                                 
026300       MOVE +1 TO VCOM-RVALUE                                             
026400       CALL DSRLSE USING VCOM-RC                                          
026500                         VCOM-DISTID                                      
026600                         VCOM-RVALUE                                      
026700       IF VCOM-RC NOT = ZERO                                              
026800         MOVE VCOM-RC TO W-RC-DISPL                                       
026900         STRING 'FEL FRÅN DSRLSE I D-CLOSE. RC=' W-RC-DISPL               
027000         DELIMITED BY SIZE INTO FELTEXT                                   
027100         CALL FELLOG                                                      
027200       END-IF                                                             
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600* IMS SEKTIONER                                                           
027700                                                                          
027800 IMS-CHANGE-SPOOL SECTION.                                                
027900                                                                          
028000     MOVE SPACE TO GODK-STATUSKODER                                       
028100     CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                       
028200                             SPOOL-OPTIONS SPOOL-FEEDBACK                 
028300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
028400     PERFORM IMS-STATUSKONTROLL                                           
028500     .                                                                    
028600     SKIP3                                                                
028700 IMS-INSERT-SPOOL SECTION.                                                
028800                                                                          
028900     MOVE SPACE TO GODK-STATUSKODER                                       
029000     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
029100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     ADD +1 TO W-RADANT                                                   
029400     .                                                                    
029500     SKIP3                                                                
029600 IMS-PURGE-SPOOL SECTION.                                                 
029700                                                                          
029800     MOVE SPACE TO GODK-STATUSKODER                                       
029900     CALL CBLTDLI USING PURG ALT-PCB                                      
030000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     EJECT                                                                
030400 IMS-STATUSKONTROLL SECTION.                                              
030500                                                                          
030600     SET STATUS-IX TO 1                                                   
030700     SEARCH GODK-STATUS                                                   
030800       AT END                                                             
030900         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
031000           TO FELTEXT                                                     
031100         CALL FELLOG                                                      
031200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031300         CONTINUE                                                         
031400     END-SEARCH                                                           
031500     .                                                                    
