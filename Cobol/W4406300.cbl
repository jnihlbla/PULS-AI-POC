000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4406300.                                    
000300 AUTHOR.                     STEFANO GIOBBI.                              
000400     DATE-WRITTEN.           MAJ 1991.                                    
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERKÖN (WDQ4) MED SB.                                        
001100*    SALDOINFORMATION LISTAS PÅ FIL.                                      
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800                                                                          
001900     SELECT W44063           ASSIGN TO      W44063D1.                     
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300     SKIP2                                                                
002400 FD  W44063                                                               
002500     LABEL RECORD STANDARD                                                
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002800*01  POST -COPY W440063 -PRE W44063-  -L.                                 
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200     SKIP2                                                                
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300*    ---- GENERELLA KONSTANTER                                            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 01  DYNAMISKA-SUBPROGRAM.                                                
003800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
003900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004100     EJECT                                                                
004800*    ---- PARAMETRAR TILL POSTSUM                                         
004900                                                                          
005000*01  -COPY W0005      -PRE POSTSUM-.                                      
005200     EJECT                                                                
005300*    ---- UTAREA FÖR W44063-POST                                          
005400                                                                          
005500 01  FILLER                      PIC X(16)   VALUE                        
005600                                             'W-W44063-POST'.             
005700     SKIP3                                                                
005800*01  AREA -COPY W440063    -PRE UT-.                                      
006000     EJECT                                                                
006100                                                                          
006200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
006300                                                                          
006400 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
006500                                                                          
006600*    ---- STATUSKOD FRÅN IMS                                              
006700                                                                          
006800 01  STATUS-WS                   PIC XX.                                  
006900     88  SEGMENT-FINNS                      VALUE '  '.                   
007000     88  SEGMENT-SLUT                       VALUE 'GB'.                   
007100                                                                          
007200 01  GODK-STATUSKODER.                                                    
007300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007400                                                                          
007500 01  SSA1                        PIC X(40).                               
007600                                                                          
007700     EJECT                                                                
008900*01      -COPY W0003.                                                     
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE                         
009300                                            'DLI-IO-AREA'.                
009400 01  DLI-IO-AREA.                                                         
009600*                                                                         
009700*  03  WLORQF01 -COPY WDQ401                                              
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100     SKIP2                                                                
010200*    -COPY W0008 -PRE WDQ4-.                                              
010400    05  FILLER                   PIC XX.                                  
010500     EJECT                                                                
010600 PROCEDURE DIVISION  USING WDQ4-PCB.                                      
010700     ENTRY 'DLITCBL' USING WDQ4-PCB.                                      
010800                                                                          
010900 STYR SECTION.                                                            
011000                                                                          
011100     PERFORM A-INIT                                                       
011200     PERFORM IMS-GET-WDQ4                                                 
011300     PERFORM UNTIL SEGMENT-SLUT                                           
011400       PERFORM B-SKRIV-UTPOST                                             
011500       PERFORM IMS-GET-WDQ4                                               
011600     END-PERFORM                                                          
011700                                                                          
011800     PERFORM Z-FINIT                                                      
011900     MOVE    ZERO TO RETURN-CODE                                          
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300 A-INIT SECTION.                                                          
012400                                                                          
012500     OPEN OUTPUT W44063                                                   
012600     MOVE 'W4406300'         TO POSTSUM-PROGNAMN                          
012700     MOVE 'W44063D1'         TO POSTSUM-DDNAMN2                           
012800     MOVE 'W44063  '         TO POSTSUM-FDNAMN                            
012900     .                                                                    
013000     EJECT                                                                
013100 B-SKRIV-UTPOST SECTION.                                                  
013200                                                                          
013310     IF ORAD-IDLEVNR = SPACE                                              
013400                                                                          
013420       IF ORAD-IDKUNDRF-RO = '0000000   ' OR                              
013430         (ORAD-IDKUNDRF-RO > '0000000   ' AND                             
013605                        ORAD-KDTPOTYP > 0 AND ORAD-TIRODAT = 0)           
013606                                                                          
013607         PERFORM BA-SKRIV                                                 
013608                                                                          
013609       END-IF                                                             
013610                                                                          
013611     END-IF                                                               
013612     .                                                                    
013613     EJECT                                                                
013614                                                                          
013615 BA-SKRIV SECTION.                                                        
013620                                                                          
013710     MOVE  ORAD-IDARTNR        TO    UT-IDARTNR                           
013800     MOVE  ORAD-IDDC           TO    UT-IDDC                              
013900     MOVE  ORAD-KDORDKL        TO    UT-KDORDKL                           
014100     IF ORAD-IDKAMPRF > +0                                                
014200         MOVE +0               TO    UT-KVBEART                           
014300     ELSE                                                                 
014400         MOVE  ORAD-KVBEART-Q  TO    UT-KVBEART                           
014500     END-IF                                                               
014600     MOVE  ORAD-KVPREAVB       TO    UT-KVPREAVB                          
014900     MOVE  ORAD-KVPRERO        TO    UT-KVPRERO                           
015000                                                                          
015100     WRITE W44063-POST         FROM  UT-AREA                              
015200                                                                          
015300     MOVE 'ORAD'               TO    POSTSUM-TRANSTYP                     
015400     CALL  POSTSUM             USING POSTSUM-PARM                         
015410     .                                                                    
015700     EJECT                                                                
015810 Z-FINIT SECTION.                                                         
015900                                                                          
016000     CLOSE W44063                                                         
016100     MOVE  'S'     TO    POSTSUM-OPKOD                                    
016200     CALL  POSTSUM USING POSTSUM-PARM                                     
016300     .                                                                    
016400     EJECT                                                                
016500*                                                                         
017900 IMS-GET-WDQ4 SECTION.                                                    
018000                                                                          
018100     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
018200     CALL    CBLTDLI          USING GN WDQ4-PCB DLI-IO-AREA               
018300     MOVE    WDQ4-STATUS-CODE TO    STATUS-WS                             
018400     PERFORM IMS-STATUSKONTROLL                                           
018500     .                                                                    
018600     SKIP3                                                                
018700 IMS-STATUSKONTROLL SECTION.                                              
018800                                                                          
018900     SET    STATUS-IX TO 1                                                
019000     SEARCH GODK-STATUS                                                   
019100       AT END CALL FELLOG                                                 
019200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
019300         CONTINUE                                                         
019400     END-SEARCH                                                           
019500     .                                                                    
