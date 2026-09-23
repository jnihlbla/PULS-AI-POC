000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4405100.                                    
000300 AUTHOR.                     STEFANO GIOBBI.                              
000400     DATE-WRITTEN.           JUN 1991.                                    
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
000910*    LÄSER NER WDQ4 M.H.A SB                                              
000911*    TILL TVÅ SEKVENSFILER.                                               
000920*                                                                         
000930*    UTFIL: W44051                                                        
000940*           W44063                                                        
000960*                                                                         
001300     EJECT                                                                
001670 ENVIRONMENT DIVISION.                                                    
001680 INPUT-OUTPUT SECTION.                                                    
001690 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800                                                                          
001900     SELECT W44051           ASSIGN TO      W44051D1.                     
001910     SELECT W44063           ASSIGN TO      W44051D2.                     
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300     SKIP2                                                                
002400 FD  W44051                                                               
002500     LABEL RECORD STANDARD                                                
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002800*01  POST -COPY W440051 -PRE W44051-  -L.                                 
003000     EJECT                                                                
003010 FD  W44063                                                               
003020     LABEL RECORD STANDARD                                                
003030     RECORDING F                                                          
003040     BLOCK CONTAINS 0.                                                    
003050*01  POST -COPY W440063 -PRE W44063-  -L.                                 
003060     EJECT                                                                
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
005300*    ---- UTAREA FÖR W44051-POST                                          
005400                                                                          
005500 01  FILLER                      PIC X(16)   VALUE                        
005600                                             'W-W44051-POST'.             
005700     SKIP3                                                                
005800*01  AREA -COPY W440051    -PRE UT-.                                      
006000     EJECT                                                                
006010*    ---- UTAREA FÖR W44063-POST                                          
006020                                                                          
006030 01  FILLER                      PIC X(16)   VALUE                        
006040                                             'W-W44063-POST'.             
006050     SKIP3                                                                
006060*01  AREA -COPY W440063    -PRE UT1-.                                     
006070     EJECT                                                                
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
008800     EJECT                                                                
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
011400       PERFORM B-SKRIV-W44051                                             
011410       PERFORM C-SKRIV-W44063                                             
011500       PERFORM IMS-GET-WDQ4                                               
011600     END-PERFORM                                                          
011700                                                                          
011800     PERFORM Z-FINIT                                                      
011900     MOVE    ZERO TO RETURN-CODE                                          
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012210                                                                          
012300 A-INIT SECTION.                                                          
012400                                                                          
012500     OPEN OUTPUT W44051                                                   
012600                 W44063                                                   
012700     MOVE 'W4405100'      TO POSTSUM-PROGNAMN                             
012900     .                                                                    
013000     EJECT                                                                
013100 B-SKRIV-W44051 SECTION.                                                  
013200                                                                          
013500     IF ORAD-TIRODAT > +0                                                 
013610                                                                          
013700       MOVE  ORAD-IDARTNR   TO    UT-IDARTNR                              
013800       MOVE  ORAD-IDDC      TO    UT-IDDC                                 
014100       MOVE  ORAD-KVBEART-Q TO    UT-KVBEART-Q                            
014200                                                                          
015100       WRITE W44051-POST    FROM  UT-AREA                                 
015200                                                                          
015220       MOVE 'W44051D1'      TO POSTSUM-DDNAMN2                            
015230       MOVE 'W44051  '      TO POSTSUM-FDNAMN                             
015300       MOVE 'RO- '          TO POSTSUM-TRANSTYP                           
015400       CALL  POSTSUM        USING POSTSUM-PARM                            
015410                                                                          
015500     END-IF                                                               
015600     .                                                                    
015601     EJECT                                                                
015602                                                                          
015610 C-SKRIV-W44063 SECTION.                                                  
015620                                                                          
015630     IF ORAD-IDLEVNR = SPACE                                              
015640      IF ORAD-IDSYSTEM NOT = 'W216'                                       
015650       IF ORAD-IDKUNDRF-RO = '0000000   ' OR                              
015660         (ORAD-IDKUNDRF-RO > '0000000   ' AND                             
015670                        ORAD-KDTPOTYP > 0 AND ORAD-TIRODAT = 0)           
015680                                                                          
015691         MOVE  ORAD-IDARTNR        TO    UT1-IDARTNR                      
015692         MOVE  ORAD-IDDC           TO    UT1-IDDC                         
015693         MOVE  ORAD-KDORDKL        TO    UT1-KDORDKL                      
015694         IF ORAD-IDKAMPRF > +0                                            
015695             MOVE +0               TO    UT1-KVBEART                      
015696         ELSE                                                             
015697* UNDANTA OKS-PREL DÅ DESSA INTE UPPDATERAR K7-OKS-BULK                   
015698             COMPUTE UT1-KVBEART = ORAD-KVBEART-Q -                       
015699                                   ORAD-KVOKS-PREL                        
015700         END-IF                                                           
015701         MOVE  ORAD-KVPREAVB       TO    UT1-KVPREAVB                     
015702         MOVE  ORAD-KVPRERO        TO    UT1-KVPRERO                      
015703                                                                          
015704         WRITE W44063-POST         FROM  UT1-AREA                         
015705                                                                          
015706         MOVE 'W44051D2'           TO    POSTSUM-DDNAMN2                  
015707         MOVE 'W44063  '           TO    POSTSUM-FDNAMN                   
015708         MOVE 'ORAD'               TO    POSTSUM-TRANSTYP                 
015709         CALL  POSTSUM             USING POSTSUM-PARM                     
015710       END-IF                                                             
015711      END-IF                                                              
015712     END-IF                                                               
015713     .                                                                    
015714     EJECT                                                                
015720                                                                          
015800 Z-FINIT SECTION.                                                         
015900                                                                          
016000     CLOSE W44051                                                         
016010           W44063                                                         
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
019100       AT END                                                             
019110         CALL FELLOG                                                      
019200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
019300         CONTINUE                                                         
019400     END-SEARCH                                                           
019500     .                                                                    
