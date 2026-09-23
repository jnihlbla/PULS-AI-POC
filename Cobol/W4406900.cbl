000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4406900.                                                
000400*AUTHOR.         STEFANO GIOBBI.                                          
000500*DATE-WRITTEN.   91/08/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        LÄSER KUNDORDERREGISTRET WDE4                                    
001200*        MED SB.                                                          
001300*        SALDOINFORMATION FRÅN WDE411 SKRIVS PÅ                           
001400*        FIL W44069.                                                      
001500*                                                                         
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- SALDOINFO FRÅN WDE411                                      
002600     SELECT W44069                     ASSIGN TO W44069D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W44069                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     SKIP2                                                                
003600*01  POST -COPY W440069 -PRE  UT-    -L.                                  
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W4406900'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100*                                                                         
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  UT-AREA-START               PIC X(24)   VALUE                        
007100                                 'UT-AREA-START  '.                       
007200     SKIP2                                                                
007300                                                                          
007400*01  AREA -COPY W440069     -PRE UT-                                      
007500     EJECT                                                                
008200     SKIP2                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  BASEN-SLUT                          VALUE 'GB'.                  
009500     SKIP3                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'WDE401-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA.                                                         
010900     03  IO-AREA                 PIC X(500) VALUE SPACE.                  
011000     SKIP3                                                                
011100     03  WDE401 REDEFINES IO-AREA.                                        
011200*        05  -COPY WDE401                                                 
011300     SKIP3                                                                
011400     03  WDE411 REDEFINES IO-AREA.                                        
011500*        05  -COPY WDE411                                                 
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
012000*01  -COPY W0008  -PRE WDE4-                                              
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING WDE4-PCB.                                      
012400     ENTRY 'DLITCBL' USING WDE4-PCB.                                      
012500                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM IMS-GN-WDE4                                                  
012800     PERFORM UNTIL BASEN-SLUT                                             
012900       IF WDE4-SEG-NAME-FB = 'WDE401  '                                   
013000         MOVE KORD-IDDC TO UT-IDDC                                        
013100       END-IF                                                             
013110       IF WDE4-SEG-NAME-FB = 'WDE411  '                                   
013120         PERFORM B-EV-SKRIV-W44069                                        
013130       END-IF                                                             
013200       PERFORM IMS-GN-WDE4                                                
013300     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600     MOVE    ZERO TO RETURN-CODE                                          
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100                                                                          
014200     OPEN OUTPUT W44069                                                   
014300                                                                          
014500     MOVE   IDPGM         TO   POSTSUM-PROGNAMN                           
014600     .                                                                    
014700     EJECT                                                                
014800 B-EV-SKRIV-W44069 SECTION.                                               
014900                                                                          
015000     IF ORAD-KVBEART NOT = ORAD-KVAVBART AND                              
015100        ORAD-KDRADSTA    < +4                                             
015300                                                                          
015400       MOVE    ORAD-IDARTNR             TO UT-IDARTNR                     
015600       MOVE    ORAD-KDORDKL             TO UT-KDORDKL                     
015710                                                                          
015800       COMPUTE UT-KVPRERO =   ORAD-KVBEART                                
015810                            - ORAD-KVAVBART                               
015820                            - ORAD-KVANNANT                               
015830                                                                          
015900       PERFORM S11-SKRIV-W44069                                           
016000                                                                          
016100     END-IF                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 Z-FINIT SECTION.                                                         
016500                                                                          
016600     CLOSE W44069                                                         
016700                                                                          
016800     MOVE 'S'     TO    POSTSUM-OPKOD                                     
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017100     EJECT                                                                
017200 S11-SKRIV-W44069 SECTION.                                                
017300                                                                          
017400     WRITE UT-POST    FROM  UT-AREA                                       
017500                                                                          
017600     MOVE 'E411'      TO    POSTSUM-TRANSTYP                              
017700     MOVE 'W44069'    TO    POSTSUM-FDNAMN                                
017800     MOVE 'W44069D1'  TO    POSTSUM-DDNAMN2                               
017900     CALL POSTSUM     USING POSTSUM-PARM                                  
018000     .                                                                    
018100     EJECT                                                                
018900*                                                                         
020300 IMS-GN-WDE4 SECTION.                                                     
020400                                                                          
020500     MOVE    '  GAGBGK'       TO     GODK-STATUSKODER                     
020600     CALL    CBLTDLI          USING  GN  WDE4-PCB DLI-IO-AREA             
020700     MOVE    WDE4-STATUS-CODE TO     STATUS-WS                            
020800     PERFORM IMS-STATUSKONTROLL                                           
020900     .                                                                    
021000     EJECT                                                                
021100 IMS-STATUSKONTROLL SECTION.                                              
021200                                                                          
021300     SET    STATUS-IX TO 1                                                
021400     SEARCH GODK-STATUS                                                   
021500       AT END                                                             
021600         MOVE 'MISSLYCKAD LÄSNING AV WDE4' TO FELTEXT-STR                 
021700         DISPLAY FELTEXT                                                  
021800         CALL    FELLOG                                                   
021900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022000         CONTINUE                                                         
022100     END-SEARCH                                                           
022200     .                                                                    
