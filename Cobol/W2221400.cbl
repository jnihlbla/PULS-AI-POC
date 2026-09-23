000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2221400.                                    
000300 AUTHOR.                     IDK INGVAR CARLSSON.                         
000400 DATE-WRITTEN.               NOVEMBER 1978.                               
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    PROGRAMMET ÄNDRAT I SAMBAND MED RASA AV HENRIK A. SEPT 1990          
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMMET TÖMMER HÄNDELSEREGISTREN WLXXBM, WLXXBY.             
001200*        DVS LÄSER OCH DELETAR SEGMENTEN MED HÄNDELSETYPERNA              
001300*        2201,2202 OCH 2233.                                              
001400*        HANTERINGEN BRYTS EFTER 500 UPPDATERINGAR, VID BRYTNING          
001500*        SKRIVS EN POST PÅ FILEN W22204 FÖR ATT EN OMSTART                
001600*        SKA GÖRAS                                                        
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLXXBM (RD3G)                              
001900*                              WLXXCK (RD3G)                              
002000*                              WLXXBY (WDG3)                              
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*--------- LEVERANSPLAN UPPDATERAD PÅ SATSARTIKEL                         
002800                                                                          
002900     SELECT W22201                 ASSIGN TO W22214D1.                    
003000                                                                          
003100*--------- PB-SEP UPPDATERAD PÅ SATSARTIKEL                               
003200                                                                          
003300     SELECT W22202                 ASSIGN TO W22214D2.                    
003400                                                                          
003500*--------- SATSSTRUKTUR UPPDATERAD M.A.P ING ARTIKEL                      
003600                                                                          
003700     SELECT W22203                 ASSIGN TO W22214D3.                    
003800                                                                          
003900*--------- KOPIA AV W22203                                                
004000                                                                          
004100     SELECT W22215                 ASSIGN TO W22214D4.                    
004200                                                                          
004300*--------- POST OM HANTERINGEN HAR AVBRUTITS                              
004400                                                                          
004500     SELECT W22204                 ASSIGN TO W22214D5.                    
004600                                                                          
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900 FILE SECTION.                                                            
005000     SKIP2                                                                
005100     EJECT                                                                
005200     SKIP3                                                                
005300 FD  W22201                                                               
005400     RECORDING F                                                          
005500     BLOCK 0                                                              
005600     LABEL RECORD STANDARD.                                               
005700                                                                          
005800*01 POST  -COPY W2222201 -PRE R2201- -L.                                  
005900     SKIP3                                                                
006000                                                                          
006100 FD  W22202                                                               
006200     RECORDING F                                                          
006300     BLOCK 0                                                              
006400     LABEL RECORD STANDARD.                                               
006500                                                                          
006600*01 POST  -COPY W2222202 -PRE R2202- -L.                                  
006700     SKIP3                                                                
006800                                                                          
006900 FD  W22203                                                               
007000     RECORDING F                                                          
007100     BLOCK 0                                                              
007200     LABEL RECORD STANDARD.                                               
007300                                                                          
007400*01 POST  -COPY W2222234 -PRE W2233- -L.                                  
007500                                                                          
007600 FD  W22215                                                               
007700     RECORDING F                                                          
007800     BLOCK 0                                                              
007900     LABEL RECORD STANDARD.                                               
008000                                                                          
008100*01 POST  -COPY W2222234 -PRE W22215- -L.                                 
008200                                                                          
008300 FD  W22204                                                               
008400     RECORDING F                                                          
008500     BLOCK 0                                                              
008600     LABEL RECORD STANDARD.                                               
008700                                                                          
008800 01  W22204-POST.                                                         
008900     03 FILLER     PIC X(1).                                              
009000                                                                          
009100     EJECT                                                                
009200 WORKING-STORAGE SECTION.                                                 
009300     SKIP3                                                                
009301                                                                          
009310*    -- CHECKED BY WY2000                                                 
009400*- - - - - - - - - - - -  PROGRAMNAMN                                     
009500 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W2221400'.                
009600                                                                          
009700*- - - - - - - - - - - -  GENERELLA KONSTANTER                            
009800                                                                          
009900 77  JA                          PIC X       VALUE 'J'.                   
010000 77  NEJ                         PIC X       VALUE 'N'.                   
010100                                                                          
010200 01  W-ANT-BEHANDLADE            PIC 9(4)    VALUE ZERO.                  
010300 01  W-MAX-BEHANDLADE            PIC 9(3)    VALUE 500.                   
010400     SKIP3                                                                
010500                                                                          
010600     EJECT                                                                
010700 01  UT-AREA-START               PIC X(24)   VALUE                        
010800                                             'UT-AREA-START'.             
010900*--------------------------------------- AREA FÖR W22201-POST             
011000*                                                                         
011100                                                                          
011200*01  AREA -COPY W2222201 -PRE R2201-                                      
011300     EJECT                                                                
011400*--------------------------------------- AREA FÖR W22202-POST             
011500*                                                                         
011600                                                                          
011700*01  AREA -COPY W2222202C0 -PRE R2202-                                    
011800     EJECT                                                                
011900*--------------------------------------- AREA FÖR W22203-POST             
012000*                                        SAMT W22215-POST                 
012100                                                                          
012200*01  AREA -COPY W2222234 -PRE W2233-                                      
012300     EJECT                                                                
012400*--------------------------------------- AREA FÖR W22204-POST             
012500*                                                                         
012600 01  W22204-AREA.                                                         
012700   03  FILLER            PIC X(1)    VALUE 'J'.                           
012800     EJECT                                                                
012900*- - - - - - - - - - - -  SUBPROGRAM                                      
013000 01  SUBPROGRAM.                                                          
013100   03  CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                    
013200   03  FELLOG            PIC X(8)    VALUE 'FELLOG  '.                    
013300   03  POSTSUM           PIC X(8)    VALUE 'POSTSUM '.                    
013400     EJECT                                                                
013500*- - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                        
013600*                                                                         
013700*01  -COPY W0005 -PRE POSTSUM-                                            
013800     EJECT                                                                
013900                                                                          
014000*- - - - - - - - - - - - - NYCKLAR TILL DLI                               
014100 01  NYCKLAR-TILL-DLI.                                                    
014200                                                                          
014300     03 W-WDG3KEY-X.                                                      
014400       05 W-IDHTYP       PIC X(4).                                        
014500       05 W-LOW-VALUE    PIC X(26)   VALUE LOW-VALUE.                     
014600                                                                          
014700     SKIP3                                                                
014800                                                                          
014900*- - - - - - - - - - - - - ARBETSAREOR TILL IMS-SEKTIONERNA               
015000                                                                          
015100 01  IMS-WS.                                                              
015200   03  FILLER            PIC X(8)    VALUE 'IMS-WS  '.                    
015300                                                                          
015400   03  STATUS-WS         PIC XX.                                          
015500     88  SEGMENT-FINNS               VALUE '  '.                          
015600     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
015700                                                                          
015800   03  GODK-STATUSKODER.                                                  
015900     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
016000                                                                          
016100   03  SSA1              PIC X(64).                                       
016200   03  SSA2              PIC X(64).                                       
016300                                                                          
016400*01  -COPY W0003                                                          
016500     SKIP3                                                                
016600*- - - - - - - - - - - - - DLI INPUT-OUTPUT AREA                          
016700                                                                          
016800 01  FILLER              PIC X(16)   VALUE 'DLI-IO-AREA'.                 
016900                                                                          
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA         PIC X(30)   VALUE SPACE.                         
017200                                                                          
017300 LINKAGE SECTION.                                                         
017400                                                                          
017500*01  -COPY W0009   -PRE MSG-                                              
017600     EJECT                                                                
017700*01  -COPY W0008   -PRE XXBM-                                             
017800     05 FILLER     PIC X(1).                                              
017900     EJECT                                                                
018000*01  -COPY W0008   -PRE XXCK-                                             
018100     05 FILLER     PIC X(1).                                              
018200     EJECT                                                                
018300*01  -COPY W0008   -PRE XXBY-                                             
018400     05 FILLER     PIC X(1).                                              
018500                                                                          
018600     EJECT                                                                
018700 PROCEDURE DIVISION USING  MSG-PCB XXBM-PCB XXCK-PCB XXBY-PCB.            
018800     ENTRY 'DLITCBL' USING MSG-PCB XXBM-PCB XXCK-PCB XXBY-PCB.            
018900                                                                          
019000     PERFORM A-INIT                                                       
019100     PERFORM B-BEHANDLA-2201                                              
019200                                                                          
019300     IF W-ANT-BEHANDLADE < W-MAX-BEHANDLADE                               
019400        PERFORM C-BEHANDLA-2202                                           
019500                                                                          
019600        IF W-ANT-BEHANDLADE < W-MAX-BEHANDLADE                            
019700           PERFORM D-BEHANDLA-2233                                        
019800        END-IF                                                            
019900     END-IF                                                               
020000                                                                          
020100     PERFORM E-AVSLUTA                                                    
020200                                                                          
020300     PERFORM Z-FINIT                                                      
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     OPEN OUTPUT W22201 W22202 W22203 W22215 W22204                       
021200                                                                          
021300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
021400     .                                                                    
021500     EJECT                                                                
021600 B-BEHANDLA-2201 SECTION.                                                 
021700******************************************************************        
021800*    BEHANDLA HÄNDELSETYP 2201                                   *        
021900*    TÖM HÄNDELSEREGISTRET PÅ SEGMENT LEVERANSPLAN UPPDATERAD    *        
022000*    PÅ SATSARTIKEL                                              *        
022100******************************************************************        
022200                                                                          
022300     MOVE '2201' TO W-IDHTYP                                              
022400     PERFORM IMS-GET-XXBM-XXBM01                                          
022500     IF SEGMENT-FINNS                                                     
022600       PERFORM IMS-GET-XXBM-XXBM11                                        
022700       PERFORM UNTIL ( SEGMENT-SAKNAS                                     
022800                      OR W-ANT-BEHANDLADE >= W-MAX-BEHANDLADE )           
023000           MOVE '2201'  TO R2201-IDHTYP                                   
023100           MOVE IO-AREA TO R2201-DATA-2201                                
023200           PERFORM S11-SKRIV-W22201                                       
023300           PERFORM IMS-DLET-XXBM                                          
023400           PERFORM IMS-GET-XXBM-XXBM11                                    
023500           ADD 1        TO W-ANT-BEHANDLADE                               
023700       END-PERFORM                                                        
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 C-BEHANDLA-2202 SECTION.                                                 
024200******************************************************************        
024300*    BEHANDLA HÄNDELSETYP 2202                                   *        
024400*    TÖM HÄNDELSEREGISTRET PÅ SEGMENT PB-SEP UPPDATERAD          *        
024500*    PÅ SATSARTIKEL                                              *        
024600******************************************************************        
024700                                                                          
024800     MOVE '2202' TO W-IDHTYP                                              
024900     PERFORM IMS-GET-XXCK-XXCK01                                          
025000     IF SEGMENT-FINNS                                                     
025100       PERFORM IMS-GET-XXCK-XXCK11                                        
025200       PERFORM UNTIL ( SEGMENT-SAKNAS                                     
025210                      OR W-ANT-BEHANDLADE >= W-MAX-BEHANDLADE )           
025400           MOVE '2202'  TO R2202-IDHTYP                                   
025500           MOVE IO-AREA TO R2202-DATA-2202                                
025600           PERFORM CA-SKRIV-W22202                                        
025700           PERFORM IMS-DLET-XXCK                                          
025800           PERFORM IMS-GET-XXCK-XXCK11                                    
025810           ADD 1        TO W-ANT-BEHANDLADE                               
026000       END-PERFORM                                                        
026100     END-IF                                                               
026200     .                                                                    
026300     SKIP2                                                                
026400 CA-SKRIV-W22202 SECTION.                                                 
026500******************************************************************        
026600*    SKRIV W22202-POST                                           *        
026700******************************************************************        
026800                                                                          
026900     WRITE R2202-POST FROM R2202-AREA                                     
027000                                                                          
027100     MOVE 'W22202'     TO POSTSUM-FDNAMN                                  
027200     MOVE 'W22214D2'   TO POSTSUM-DDNAMN2                                 
027300     MOVE R2202-IDHTYP TO POSTSUM-TRANSTYP                                
027400     CALL POSTSUM USING POSTSUM-PARM                                      
027500     .                                                                    
027600     SKIP2                                                                
027700     EJECT                                                                
027800 D-BEHANDLA-2233 SECTION.                                                 
027900******************************************************************        
028000*    BEHANDLA HÄNDELSETYP 2233                                   *        
028100*    TÖM HÄNDELSEREGISTRET PÅ SEGMENT SATSSTRUKTUR UPPDATERAD    *        
028200*    M.A.P. ING ARTIKEL                                          *        
028300*    SKRIV PÅ 2 UTFILER PGA SENARE HANTERING I BMP-PROGRAM       *        
028400******************************************************************        
028500                                                                          
028600     MOVE '2233' TO W-IDHTYP                                              
028700     PERFORM IMS-GET-XXBY-XXBY01                                          
028800     IF SEGMENT-FINNS                                                     
028900       PERFORM IMS-GET-XXBY-XXBY11                                        
029000       PERFORM UNTIL ( SEGMENT-SAKNAS                                     
029100                      OR W-ANT-BEHANDLADE >= W-MAX-BEHANDLADE )           
029300           MOVE '2233'  TO W2233-IDHTYP                                   
029400           MOVE IO-AREA TO W2233-DATA-2234                                
029500           PERFORM S12-SKRIV-W22203                                       
029600           PERFORM S13-SKRIV-W22215                                       
029700           PERFORM IMS-DLET-XXBY                                          
029800           PERFORM IMS-GET-XXBY-XXBY11                                    
029900           ADD 1        TO W-ANT-BEHANDLADE                               
030100       END-PERFORM                                                        
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 E-AVSLUTA SECTION.                                                       
030600                                                                          
030700*****************************************************************         
030800*    SKRIVER POST PÅ FIL W22204 OM BEHANDLINGEN HAR AVBRUTITS             
030900*    DETTA FÖR ATT SE ATT OMSTART SKA GÖRAS.                              
031000*****************************************************************         
031100                                                                          
031200     IF SEGMENT-FINNS                                                     
031300        PERFORM S14-SKRIV-W22204                                          
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 Z-FINIT SECTION.                                                         
031800                                                                          
031900     CLOSE W22201                                                         
032000           W22202                                                         
032100           W22203                                                         
032200           W22215                                                         
032300           W22204                                                         
032400                                                                          
032500     MOVE 'S' TO POSTSUM-OPKOD                                            
032600     CALL POSTSUM USING POSTSUM-PARM                                      
032700     .                                                                    
032800     EJECT                                                                
032900 S11-SKRIV-W22201 SECTION.                                                
033000******************************************************************        
033100*    SKRIV W22201-POST                                           *        
033200******************************************************************        
033300                                                                          
033400     WRITE R2201-POST FROM R2201-AREA                                     
033500                                                                          
033600     MOVE 'W22201'     TO POSTSUM-FDNAMN                                  
033700     MOVE 'W22214D1'   TO POSTSUM-DDNAMN2                                 
033800     MOVE R2201-IDHTYP TO POSTSUM-TRANSTYP                                
033900     CALL POSTSUM USING POSTSUM-PARM                                      
034000     .                                                                    
034100     EJECT                                                                
034200 S12-SKRIV-W22203 SECTION.                                                
034300******************************************************************        
034400*    SKRIV W22203-POST                                           *        
034500******************************************************************        
034600                                                                          
034700     WRITE W2233-POST FROM W2233-AREA                                     
034800                                                                          
034900     MOVE 'W22203'     TO POSTSUM-FDNAMN                                  
035000     MOVE 'W22214D2'   TO POSTSUM-DDNAMN2                                 
035100     MOVE W2233-IDHTYP TO POSTSUM-TRANSTYP                                
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035300     .                                                                    
035400     EJECT                                                                
035500 S13-SKRIV-W22215 SECTION.                                                
035600******************************************************************        
035700*    SKRIV W22215-POST                                           *        
035800******************************************************************        
035900                                                                          
036000     WRITE W22215-POST FROM W2233-AREA                                    
036100                                                                          
036200     MOVE 'W22215'     TO POSTSUM-FDNAMN                                  
036300     MOVE 'W22214D3'   TO POSTSUM-DDNAMN2                                 
036400     MOVE W2233-IDHTYP TO POSTSUM-TRANSTYP                                
036500     CALL POSTSUM USING POSTSUM-PARM                                      
036600     .                                                                    
036700     EJECT                                                                
036800 S14-SKRIV-W22204 SECTION.                                                
036900******************************************************************        
037000*    SKRIV W22204-POST                                           *        
037100******************************************************************        
037200                                                                          
037300     WRITE W22204-POST FROM W22204-AREA                                   
037400                                                                          
037500     MOVE 'W22204'     TO POSTSUM-FDNAMN                                  
037600     MOVE 'W22214D4'   TO POSTSUM-DDNAMN2                                 
037700     CALL POSTSUM USING POSTSUM-PARM                                      
037800     .                                                                    
037900     EJECT                                                                
038000* --- IMS SEKTIONER ---                                                   
038100 SKIP3                                                                    
038200 IMS-GET-XXBM-XXBM01 SECTION.                                             
038300                                                                          
038400     STRING 'WLXXBM01(WDG3KEY  =' W-WDG3KEY-X ')'                         
038500            DELIMITED BY SIZE INTO SSA1                                   
038600     MOVE '  ' TO GODK-STATUSKODER                                        
038700     CALL CBLTDLI USING GU XXBM-PCB DLI-IO-AREA SSA1                      
038800     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     SKIP3                                                                
039200 IMS-GET-XXBM-XXBM11 SECTION.                                             
039300                                                                          
039400     MOVE 'WLXXBM11 ' TO SSA1                                             
039500     MOVE '  GE' TO GODK-STATUSKODER                                      
039600     CALL CBLTDLI USING GHNP XXBM-PCB DLI-IO-AREA SSA1                    
039700     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     SKIP3                                                                
040100 IMS-DLET-XXBM SECTION.                                                   
040200                                                                          
040300     MOVE '    ' TO GODK-STATUSKODER                                      
040400     CALL CBLTDLI USING DLET XXBM-PCB DLI-IO-AREA                         
040500     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-GET-XXBY-XXBY01 SECTION.                                             
041000                                                                          
041100     STRING 'WLXXBY01(WDG3KEY  =' W-WDG3KEY-X ')'                         
041200            DELIMITED BY SIZE INTO SSA1                                   
041300     MOVE '  ' TO GODK-STATUSKODER                                        
041400     CALL CBLTDLI USING GU XXBY-PCB DLI-IO-AREA SSA1                      
041500     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
041600     PERFORM IMS-STATUSKONTROLL                                           
041700     .                                                                    
041800     SKIP3                                                                
041900 IMS-GET-XXBY-XXBY11 SECTION.                                             
042000                                                                          
042100     MOVE 'WLXXBY11 ' TO SSA1                                             
042200     MOVE '  GE' TO GODK-STATUSKODER                                      
042300     CALL CBLTDLI USING GHNP XXBY-PCB DLI-IO-AREA SSA1                    
042400     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     SKIP3                                                                
042800 IMS-DLET-XXBY SECTION.                                                   
042900                                                                          
043000     MOVE '    ' TO GODK-STATUSKODER                                      
043100     CALL CBLTDLI USING DLET XXBY-PCB DLI-IO-AREA                         
043200     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     EJECT                                                                
043600 IMS-GET-XXCK-XXCK01 SECTION.                                             
043700                                                                          
043800     STRING 'WLXXCK01(WDG3KEY  =' W-WDG3KEY-X ')'                         
043900            DELIMITED BY SIZE INTO SSA1                                   
044000     MOVE '  ' TO GODK-STATUSKODER                                        
044100     CALL CBLTDLI USING GU XXCK-PCB DLI-IO-AREA SSA1                      
044200     MOVE XXCK-STATUS-CODE TO STATUS-WS                                   
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500     SKIP3                                                                
044600 IMS-GET-XXCK-XXCK11 SECTION.                                             
044700                                                                          
044800     MOVE 'WLXXCK11 ' TO SSA1                                             
044900     MOVE '  GE' TO GODK-STATUSKODER                                      
045000     CALL CBLTDLI USING GHNP XXCK-PCB DLI-IO-AREA SSA1                    
045100     MOVE XXCK-STATUS-CODE TO STATUS-WS                                   
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     .                                                                    
045400     SKIP3                                                                
045500 IMS-DLET-XXCK SECTION.                                                   
045600                                                                          
045700     MOVE '    ' TO GODK-STATUSKODER                                      
045800     CALL CBLTDLI USING DLET XXCK-PCB DLI-IO-AREA                         
045900     MOVE XXCK-STATUS-CODE TO STATUS-WS                                   
046000     PERFORM IMS-STATUSKONTROLL                                           
046100     .                                                                    
046200     EJECT                                                                
046300 IMS-STATUSKONTROLL SECTION.                                              
046400     SET STATUS-IX TO 1                                                   
046500     SEARCH GODK-STATUS                                                   
046600       AT END CALL FELLOG                                                 
046700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046800     END-SEARCH                                                           
046900     .                                                                    
