000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4262700.                                        
000400 AUTHOR.                 GERRY CARMICHAEL                                 
000500 DATE-WRITTEN.           MAR 1992.                                        
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
000910*        KVALITET  KONTROLLRAPPORT                                        
000920*        RENSNING AV W6H7 KONTROLLRAPPORTREGISTER                         
000940*                                                                         
000950*        INDATA.  FIL:  W42650                                            
000960*                                                                         
000970*        UTDATA.  FIL:  W42627                                            
000980*                                                                         
000990*        UPPDATERAR           W6KVAE (W6H7)                               
000991*                             W6CKPC (W6G2 CHECKPOINT)                    
001000     EJECT                                                                
001100 ENVIRONMENT DIVISION.                                                    
001200                                                                          
001300 INPUT-OUTPUT SECTION.                                                    
001400                                                                          
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700     SELECT W42650 ASSIGN TO W42627D1.                                    
001710     SELECT W42627 ASSIGN TO W42627D2.                                    
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000                                                                          
002100 FILE SECTION.                                                            
002200     SKIP3                                                                
002210 FD  W42650                                                               
002220     LABEL RECORD STANDARD                                                
002230     RECORDING F                                                          
002240     BLOCK CONTAINS 0.                                                    
002250 01  W42650-POST.                                                         
002270*    03  -COPY W4265001 -L.                                               
002292     EJECT                                                                
002300 FD  W42627                                                               
002400     LABEL RECORD STANDARD                                                
002500     RECORDING V                                                          
002600     BLOCK CONTAINS 0.                                                    
002700 01  W6H701-POST.                                                         
002701     03  W6H701-IDPTYP  PIC X(3).                                         
002702*    03  -COPY W6H701 -L.                                                 
002703 01  W6H711-POST.                                                         
002704     03  W6H711-IDPTYP  PIC X(3).                                         
002705*    03  -COPY W6H711 -L.                                                 
002706 01  W6H712-POST.                                                         
002707     03  W6H712-IDPTYP  PIC X(3).                                         
002708*    03  -COPY W6H712 -L.                                                 
002709 01  W6H713-POST.                                                         
002710     03  W6H713-IDPTYP  PIC X(3).                                         
002720*    03  -COPY W6H713 -L.                                                 
002730 01  W6H714-POST.                                                         
002740     03  W6H714-IDPTYP  PIC X(3).                                         
002750*    03  -COPY W6H714 -L.                                                 
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000     SKIP2                                                                
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100*    ---- ARBETSVARIABLER                                                 
003200*                                                                         
003201 77  IDPGM                       PIC X(8)    VALUE 'W4262700'.            
003202 01  FELTEXT.                                                             
003203     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003204     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003210 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
003220 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
003230 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
003240 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
003250 77  CHKP-ANT                    PIC S9(7)   COMP-3.                      
003260 77  CHKP-ANT-MAX                PIC S9(7)   COMP-3 VALUE +500.           
003270 77  POST-ANT                    PIC S9(7)   COMP-3.                      
003280 77  JA                          PIC X       VALUE 'J'.                   
003290 77  NEJ                         PIC X       VALUE 'N'.                   
003291 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP SYNC VALUE ZERO.        
003296*                                                                         
003297     EJECT                                                                
003298 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003299 01  FILLER REDEFINES DAGENS-DATUM.                                       
003300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
003301     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
003302     03  DAGENS-DATUM-DAG        PIC 9(2).                                
003303     EJECT                                                                
004140                                                                          
004150 01  W42650-EOF-SW       PIC X      VALUE 'N'.                            
004160     88  END-OF-W42650              VALUE 'J'.                            
004170                                                                          
004600     SKIP2                                                                
005000*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
005100     SKIP3                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
005310   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
005400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005510   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
005600   EJECT                                                                  
005610*  --- PARAMETRAR TILL POSTSUM                                            
005620*                                                                         
005630*01 -COPY W0005  -PRE POSTSUM-                                            
005640   EJECT                                                                  
005700*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
005840                                                                          
005842*  03 -COPY W4265001 -PRE IN-                                             
005843 01 KVAE01-POST.                                                          
005860   03  KVAE01-KR-IDPTYP      PIC X(3).                                    
005861*  03 -COPY W6H701 -PRE KVAE01-                                           
005870     SKIP3                                                                
005871 01 KVAE11-POST.                                                          
005872   03  KVAE11-KOLL-IDPTYP    PIC X(3).                                    
005880   03 -COPY W6H711 -PRE KVAE11-                                           
005881     SKIP3                                                                
005882 01 KVAE12-POST.                                                          
005883   03  KVAE12-EK-IDPTYP      PIC X(3).                                    
005884*  03 -COPY W6H712 -PRE KVAE12-                                           
005885     SKIP3                                                                
005886 01 KVAE13-POST.                                                          
005887   03  KVAE13-JUST-IDPTYP    PIC X(3).                                    
005888   03 -COPY W6H713 -PRE KVAE13-                                           
005889     SKIP3                                                                
005890 01 KVAE14-POST.                                                          
005891   03  KVAE14-TEXT-IDPTYP    PIC X(3).                                    
005892   03 -COPY W6H714 -PRE KVAE14-                                           
005893                                                                          
005900*    ---- PARAMETRAR TILL DATUMKORT                                       
006010                                                                          
006020 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
006030     SKIP3                                                                
006040*    -COPY WDATAREAC0                                                     
006050*++INCLUDE WDATAREAC0                                                     
006060     EJECT                                                                
006100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
006200                                                                          
006300 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
006400     SKIP3                                                                
006500*    ---- STATUSKOD FRÅN IMS                                              
006600                                                                          
006700 01  STATUS-WS               PIC XX.                                      
006800     88  SEGMENT-SLUT                     VALUE 'GB'.                     
006900     88  SEGMENT-FINNS                    VALUE '  '.                     
007000     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
007100     88  IMS-EJ-OK                        VALUE 'XD'.                     
007200     SKIP3                                                                
007300 01  GODK-STATUSKODER.                                                    
007400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007500     SKIP3                                                                
007600 01  SSA1                    PIC X(64).                                   
007610 01  SSA2                    PIC X(64).                                   
007700     EJECT                                                                
007800*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
007900                                                                          
008000 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
008100 01  NYCKLAR-TILL-DLI.                                                    
008200                                                                          
008300   03  W-IDKR-X.                                                          
008400     05  W-IDKR              PIC 9(5)  VALUE ZERO.                        
008700                                                                          
008800   03  W-IDHTYP-X.                                                        
008900     05  W-IDHTYP            PIC X(4)    VALUE SPACE.                     
009000     05  NYCKEL-VALFRI       PIC X(26).                                   
009010                                                                          
009100     EJECT                                                                
009200*01  -COPY W0003                                                          
009300     EJECT                                                                
009400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
009500     SKIP3                                                                
009600 01  DLI-IO-AREA.                                                         
009700   03  IO-AREA               PIC X(1000).                                 
009800     SKIP3                                                                
009900   03  W6KVAE01 REDEFINES IO-AREA.                                        
009910*      05 -COPY W6H701                                                    
009920     SKIP3                                                                
009930   03  W6KVAE11 REDEFINES IO-AREA.                                        
009940*      05 -COPY W6H711                                                    
009941     SKIP3                                                                
009950   03  W6KVAE12 REDEFINES IO-AREA.                                        
009960*      05 -COPY W6H712                                                    
009970     SKIP3                                                                
009980   03  W6KVAE13 REDEFINES IO-AREA.                                        
009990*      05 -COPY W6H713                                                    
010000     EJECT                                                                
010010   03  W6KVAE14 REDEFINES IO-AREA.                                        
010020*      05 -COPY W6H714                                                    
010030     EJECT                                                                
010100 01  DLI-IO-AREA-CKPC.                                                    
010200   03  W6CKPC11.                                                          
010201*      05  -COPY W6GX6020                                                 
010202     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400     SKIP2                                                                
010500*01  -COPY W0009      -PRE  MSG-                                          
010600     EJECT                                                                
010700*01  -COPY W0008      -PRE  KVAE-                                         
010800       05  FILLER                PIC X.                                   
010900     EJECT                                                                
010910*01  -COPY W0008      -PRE  CKPC-                                         
010920       05  FILLER                PIC X.                                   
010930     EJECT                                                                
011000 PROCEDURE DIVISION  USING MSG-PCB KVAE-PCB CKPC-PCB.                     
011100     ENTRY 'DLITCBL' USING MSG-PCB KVAE-PCB CKPC-PCB.                     
011110                                                                          
011120     PERFORM A-INIT                                                       
011130                                                                          
011140     PERFORM IMS-RESTART                                                  
011150                                                                          
011160     PERFORM IMS-LAS-ATERSTART                                            
011170                                                                          
011180     IF 6020-KVPOST > +0                                                  
011190       PERFORM B-LAS-FRAM-TILL-CHKPOINT                                   
011191     ELSE                                                                 
011192       PERFORM S01-LAES-W42650                                            
011193     END-IF                                                               
011194                                                                          
011195     PERFORM UNTIL END-OF-W42650                                          
011196                                                                          
011197       PERFORM C-INITIERA-KEYVALUE                                        
011198                                                                          
011199       PERFORM D-BEHANDLA-INDATA                                          
011207                                                                          
011208       PERFORM S01-LAES-W42650                                            
011209                                                                          
011210     END-PERFORM                                                          
011211                                                                          
011212                                                                          
011213     PERFORM Z-FINIT                                                      
011214                                                                          
011215     MOVE ZERO TO RETURN-CODE                                             
011216     GOBACK                                                               
011217     .                                                                    
011218     EJECT                                                                
011219 A-INIT SECTION.                                                          
011220     SKIP2                                                                
011221                                                                          
011222     OPEN INPUT W42650                                                    
011223                                                                          
011224     OPEN OUTPUT W42627                                                   
011225                                                                          
011226     ACCEPT DAGENS-DATUM       FROM DATE                                  
011227                                                                          
011228     MOVE +0                   TO CHKP-ANT                                
011229                                  POST-ANT                                
011230     MOVE '6019'               TO W-IDHTYP                                
011231     MOVE LOW-VALUE            TO NYCKEL-VALFRI                           
011232     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011233     .                                                                    
011234     EJECT                                                                
011235 B-LAS-FRAM-TILL-CHKPOINT SECTION.                                        
011236                                                                          
011237     PERFORM UNTIL END-OF-W42650 OR                                       
011238                       POST-ANT = 6020-KVPOST                             
011239        ADD +1           TO POST-ANT                                      
011240        PERFORM S01-LAES-W42650                                           
011241     END-PERFORM                                                          
011242                                                                          
011243     IF END-OF-W42650                                                     
011244        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
011245                      TO FELTEXT                                          
011246        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
011247     END-IF                                                               
011248     .                                                                    
011249     EJECT                                                                
011250                                                                          
011251                                                                          
011252 C-INITIERA-KEYVALUE SECTION.                                             
011253     ADD +1                  TO POST-ANT                                  
011254                                CHKP-ANT                                  
011255                                                                          
011256     IF CHKP-ANT > CHKP-ANT-MAX                                           
011257       PERFORM CA-TAG-CHECKPOINT                                          
011258       MOVE +0               TO CHKP-ANT                                  
011259     END-IF                                                               
011260                                                                          
011261     MOVE IN-IDKR            TO W-IDKR                                    
011262     .                                                                    
011263     EJECT                                                                
011264 CA-TAG-CHECKPOINT SECTION.                                               
011265* --- UPPDATERA ÅTERSTARTSREGISTRET                                       
011266                                                                          
011267     PERFORM IMS-LAS-ATERSTART                                            
011268     MOVE POST-ANT           TO 6020-KVPOST                               
011269     ACCEPT 6020-TIUPPDAT    FROM DATE                                    
011270     ACCEPT 6020-TIUPPTID    FROM TIME                                    
011271     PERFORM IMS-REPL-ATERSTART                                           
011272     PERFORM IMS-CHECKPOINT                                               
011273     .                                                                    
011274     EJECT                                                                
011275 D-BEHANDLA-INDATA SECTION.                                               
011276                                                                          
011277     PERFORM IMS-GU-W6KVAE01                                              
011278     PERFORM DA-LAES-BARNA-SKRIV-POSTER                                   
011279     PERFORM IMS-GHU-W6KVAE01                                             
011280     PERFORM IMS-DLET-W6KVAE01                                            
011281     .                                                                    
011282     EJECT                                                                
011290                                                                          
015000 DA-LAES-BARNA-SKRIV-POSTER SECTION.                                      
015110                                                                          
015120     MOVE '701'       TO  KVAE01-KR-IDPTYP                                
015200     MOVE KR-W6H701   TO  KVAE01-KR-W6H701                                
015210     WRITE W6H701-POST FROM KVAE01-POST                                   
015211                                                                          
015212     MOVE '701'        TO POSTSUM-TRANSTYP                                
015213     MOVE 'W42627 ' TO POSTSUM-FDNAMN                                     
015214     MOVE 'W42627D2' TO POSTSUM-DDNAMN2                                   
015215     CALL POSTSUM USING POSTSUM-PARM                                      
015220                                                                          
015230     PERFORM IMS-GNP-W6KVAE11                                             
015250     PERFORM UNTIL SEGMENT-SAKNAS                                         
015251        MOVE '711'       TO  KVAE11-KOLL-IDPTYP                           
015252        MOVE KOLL-W6H711 TO  KVAE11-KOLL-W6H711                           
015253        WRITE W6H711-POST FROM KVAE11-POST                                
015254                                                                          
015255        MOVE '711'     TO POSTSUM-TRANSTYP                                
015256        MOVE 'W42627 ' TO POSTSUM-FDNAMN                                  
015257        MOVE 'W42627D2' TO POSTSUM-DDNAMN2                                
015258        CALL POSTSUM USING POSTSUM-PARM                                   
015259                                                                          
015261        PERFORM IMS-GNP-W6KVAE11                                          
015262     END-PERFORM                                                          
015270                                                                          
015271     PERFORM IMS-GNP-W6KVAE12                                             
015280     PERFORM UNTIL SEGMENT-SAKNAS                                         
015290        MOVE '712'       TO  KVAE12-EK-IDPTYP                             
015291        MOVE EK-W6H712   TO  KVAE12-EK-W6H712                             
015292        WRITE W6H712-POST FROM KVAE12-POST                                
015293                                                                          
015294        MOVE '712'     TO POSTSUM-TRANSTYP                                
015295        MOVE 'W42627 ' TO POSTSUM-FDNAMN                                  
015296        MOVE 'W42627D2' TO POSTSUM-DDNAMN2                                
015297        CALL POSTSUM USING POSTSUM-PARM                                   
015298                                                                          
015299        PERFORM IMS-GNP-W6KVAE12                                          
015300     END-PERFORM                                                          
015301                                                                          
015302     PERFORM IMS-GNP-W6KVAE13                                             
015303     PERFORM UNTIL SEGMENT-SAKNAS                                         
015304        MOVE '713'       TO  KVAE13-JUST-IDPTYP                           
015305        MOVE JUST-W6H713 TO  KVAE13-JUST-W6H713                           
015306        WRITE W6H713-POST FROM KVAE13-POST                                
015307                                                                          
015308        MOVE '713'     TO POSTSUM-TRANSTYP                                
015309        MOVE 'W42627 ' TO POSTSUM-FDNAMN                                  
015310        MOVE 'W42627D2' TO POSTSUM-DDNAMN2                                
015311        CALL POSTSUM USING POSTSUM-PARM                                   
015312                                                                          
015313        PERFORM IMS-GNP-W6KVAE13                                          
015314     END-PERFORM                                                          
015315                                                                          
015316     PERFORM IMS-GNP-W6KVAE14                                             
015317     PERFORM UNTIL SEGMENT-SAKNAS                                         
015318        MOVE '714'           TO  KVAE14-TEXT-IDPTYP                       
015319        MOVE TEXT-W6H714-CTX TO  KVAE14-TEXT-W6H714-CTX                   
015320        WRITE W6H714-POST  FROM  KVAE14-POST                              
015321                                                                          
015322        MOVE '714'     TO POSTSUM-TRANSTYP                                
015323        MOVE 'W42627 ' TO POSTSUM-FDNAMN                                  
015324        MOVE 'W42627D2' TO POSTSUM-DDNAMN2                                
015325        CALL POSTSUM USING POSTSUM-PARM                                   
015326                                                                          
015327        PERFORM IMS-GNP-W6KVAE14                                          
015328     END-PERFORM                                                          
015329                                                                          
015330     .                                                                    
015400     EJECT                                                                
015410 S01-LAES-W42650  SECTION.                                                
015420     SKIP2                                                                
015430     READ W42650 INTO IN-W4265001                                         
015440     AT END                                                               
015450        SET END-OF-W42650 TO TRUE                                         
015460                                                                          
015470     NOT AT END                                                           
015480        MOVE 'W42650' TO POSTSUM-FDNAMN                                   
015490        MOVE 'W42650D1' TO POSTSUM-DDNAMN2                                
015491        MOVE '001'      TO POSTSUM-TRANSTYP                               
015492        CALL POSTSUM USING POSTSUM-PARM                                   
015493                                                                          
015494     END-READ                                                             
015495     .                                                                    
015496     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015510                                                                          
015520                                                                          
015530     CLOSE W42650                                                         
015540                                                                          
015550           W42627                                                         
015560                                                                          
015570     SKIP2                                                                
015580*--  NOLLA ÅTERSTARTSINFO                                                 
015590                                                                          
015591     PERFORM IMS-LAS-ATERSTART                                            
015592                                                                          
015593     MOVE +0           TO 6020-KVPOST                                     
015594     ACCEPT 6020-TIUPPDAT FROM DATE                                       
015595     ACCEPT 6020-TIUPPTID FROM TIME                                       
015596                                                                          
015597     PERFORM IMS-REPL-ATERSTART                                           
015598                                                                          
015599     MOVE 'S' TO POSTSUM-OPKOD                                            
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015601     .                                                                    
015602     EJECT                                                                
016400*    ---- IMS SEKTIONER                                                   
016500                                                                          
016510 IMS-GU-W6KVAE01 SECTION.                                                 
016511     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
016512          DELIMITED BY SIZE INTO SSA1                                     
016540     MOVE '  ' TO GODK-STATUSKODER                                        
016550     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA SSA1                      
016560     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
016570     PERFORM IMS-STATUSKONTROLL                                           
016580     .                                                                    
016590     EJECT                                                                
016600 IMS-GHU-W6KVAE01 SECTION.                                                
016700     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
016800          DELIMITED BY SIZE INTO SSA1                                     
016900     MOVE '  ' TO GODK-STATUSKODER                                        
017000     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA SSA1                     
017100     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
017200     PERFORM IMS-STATUSKONTROLL                                           
017300     .                                                                    
017400     EJECT                                                                
017491 IMS-GNP-W6KVAE11 SECTION.                                                
017492     MOVE 'W6KVAE11  ' TO SSA1                                            
017494     MOVE '  GE' TO GODK-STATUSKODER                                      
017495     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA SSA1                     
017496     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
017497     PERFORM IMS-STATUSKONTROLL                                           
017498     .                                                                    
017499     EJECT                                                                
017500 IMS-GNP-W6KVAE12 SECTION.                                                
017501     MOVE 'W6KVAE12  ' TO SSA1                                            
017502     MOVE '  GE' TO GODK-STATUSKODER                                      
017503     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA SSA1                     
017504     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
017505     PERFORM IMS-STATUSKONTROLL                                           
017506     .                                                                    
017507     EJECT                                                                
017508 IMS-GNP-W6KVAE13 SECTION.                                                
017509     MOVE 'W6KVAE13  ' TO SSA1                                            
017510     MOVE '  GE' TO GODK-STATUSKODER                                      
017511     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA SSA1                     
017512     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
017513     PERFORM IMS-STATUSKONTROLL                                           
017514     .                                                                    
017515     EJECT                                                                
017516 IMS-GNP-W6KVAE14 SECTION.                                                
017517     MOVE 'W6KVAE14  ' TO SSA1                                            
017518     MOVE '  GE' TO GODK-STATUSKODER                                      
017519     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA SSA1                     
017520     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
017521     PERFORM IMS-STATUSKONTROLL                                           
017522     .                                                                    
017523     EJECT                                                                
017530 IMS-DLET-W6KVAE01 SECTION.                                               
017600                                                                          
017700     MOVE '  ' TO GODK-STATUSKODER                                        
017800     CALL CBLTDLI USING DLET KVAE-PCB DLI-IO-AREA                         
017900     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
018000     PERFORM IMS-STATUSKONTROLL                                           
018100     .                                                                    
018200     EJECT                                                                
018300 IMS-RESTART SECTION.                                                     
018400     SKIP2                                                                
018500     MOVE SPACE TO MSG-IO-AREA                                            
018600     MOVE '  ' TO GODK-STATUSKODER                                        
018700     CALL CBLTDLI USING XRST MSG-PCB                                      
018800                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
018900                        CHKP-AREA-LENGTH CHKP-AREA                        
019000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019100     PERFORM IMS-STATUSKONTROLL                                           
019200     .                                                                    
019300     EJECT                                                                
019400 IMS-CHECKPOINT SECTION.                                                  
019500     SKIP2                                                                
019600     MOVE SPACE TO MSG-IO-AREA                                            
019700     MOVE '  XD' TO GODK-STATUSKODER                                      
019800     CALL CBLTDLI USING CHKP MSG-PCB                                      
019900                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
020000                        CHKP-AREA-LENGTH CHKP-AREA                        
020100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020200     PERFORM IMS-STATUSKONTROLL                                           
020300                                                                          
020400     IF IMS-EJ-OK                                                         
020500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
020600       DISPLAY FELTEXT                                                    
020700       CALL FELLOG                                                        
020800     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 IMS-LAS-ATERSTART SECTION.                                               
021200     SKIP2                                                                
021300     MOVE '6019'         TO W-IDHTYP                                      
021400     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
021500     STRING 'W6CKPC01(W6GXKEY  =' W-IDHTYP-X ')'                          
021600                    DELIMITED BY SIZE INTO SSA1                           
021700     MOVE 'W6CKPC11 '    TO SSA2                                          
021800     MOVE '  '           TO GODK-STATUSKODER                              
021900     CALL CBLTDLI USING GHU CKPC-PCB DLI-IO-AREA-CKPC SSA1 SSA2           
022000     MOVE CKPC-STATUS-CODE TO STATUS-WS                                   
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022300                                                                          
022400 IMS-REPL-ATERSTART SECTION.                                              
022500     SKIP2                                                                
022600     MOVE '  '             TO GODK-STATUSKODER                            
022700     CALL CBLTDLI USING REPL CKPC-PCB DLI-IO-AREA-CKPC                    
022800     MOVE CKPC-STATUS-CODE TO STATUS-WS                                   
022900     PERFORM IMS-STATUSKONTROLL                                           
023000     .                                                                    
023010                                                                          
023020 IMS-STATUSKONTROLL SECTION.                                              
023030     SKIP2                                                                
023040     SET STATUS-IX TO 1                                                   
023050     SEARCH GODK-STATUS                                                   
023060       AT END CALL FELLOG                                                 
023070       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
023080     END-SEARCH                                                           
023090     .                                                                    
023091                                                                          
