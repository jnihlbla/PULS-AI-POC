000101**********************************************************                
000202 ID DIVISION.                                                             
000302 PROGRAM-ID.     W4043100.                                                
000402 AUTHOR.         MARKUS ASPFJÄLL.                                         
000502 DATE-WRITTEN.   99/02/12.                                                
000602 DATE-COMPILED.                                                           
000702                                                                          
000802*    FUNKTION:                                                            
000902*        LÄSER BAS WDF1                                                   
001002*        OCH UPPDATERAR WDF118 SEGMENTET MED                              
001102*                                                                         
001202*        PROGRAMMET UPPDATERAR WLLEVA (WDF1)                              
001302*                              WDF601                                     
001402*                              WDQ211                                     
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T431 W4T431U                                      
001800*        MID:         W4I43101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O43101                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4043100'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003802 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003902 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004002                                                                          
004100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500                                                                          
004600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004700     88  INDATA-OK                           VALUE 'J'.                   
004800     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005100     88  ALLT-OK                             VALUE 'J'.                   
005200     88  ALLT-FEL                            VALUE 'N'.                   
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  VISA-SW                     PIC X       VALUE 'J'.                   
005900     88  VISA-OK                             VALUE 'J'.                   
006000     88  VISA-FEL                            VALUE 'N'.                   
006100                                                                          
006110 77  DELETE-SW                   PIC X       VALUE 'N'.                   
006120     88  DELETE-OK                           VALUE 'J'.                   
006140                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  EGEN-MID                            VALUE '4431'.                
006400     88  GODK-MID                            VALUE '4431' '4432'          
006500                                                   '4433' '4434'          
006600                                                   '4435' '4436'          
006700                                                   '4437' '4438'          
006800                                                   '4439'.                
006900     88  HELP-MID                            VALUE '0551'.                
007000                                                                          
007100 01  W-TIMINUT-COMP              PIC S999V99  COMP-3.                     
007200 01  W-TIMINUT                   PIC S99V99.                              
007300 01  WS-TIMINUT                  PIC  9(5).                               
007400 01  TID.                                                                 
007500     03 FILLER                   PIC 9.                                   
007600     03 BRYT-TIMME               PIC 9(2).                                
007700        88 TIMME-OK                          VALUE 0 THRU 23.             
007800     03 BRYT-MINUT               PIC 9(2).                                
007900        88 MINUT-OK                          VALUE 0 THRU 59.             
008000                                                                          
008102 01  WS-CURRENT-DATE              PIC 9(8)   VALUE ZERO.                  
008202                                                                          
008302 01  WS-DASNDDAT                  PIC 9(8)   VALUE 20000000.              
008402 01  FILLER REDEFINES WS-DASNDDAT.                                        
008502     03  FILLER                   PIC 9(2).                               
008602     03  WS-TISNDDAT              PIC 9(6).                               
008702                                                                          
008802 01  WS-DASKEPPN                  PIC 9(8)   VALUE 20000000.              
008902 01  FILLER REDEFINES WS-DASKEPPN.                                        
009002     03  FILLER                   PIC 9(2).                               
009102     03  WS-TISKEPPN              PIC 9(6).                               
009202                                                                          
009203 01  WS-DSTY-KVDAGAR-DIFF         PIC S9(3)  VALUE ZERO.                  
009204 01  WS-DSTY-KVDAGAR-TPO          PIC S9(3)  VALUE ZERO.                  
009205                                                                          
009300     EJECT                                                                
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDC99                                                       
009700       EJECT                                                              
009800 01    TEST-IDDISTR              PIC 9(5)    COMP-3.                      
009900 01    FILLER REDEFINES TEST-IDDISTR.                                     
010000*    03   -COPY WWDIST03.                                                 
010100     EJECT                                                                
010200                                                                          
010300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010400 01  GENERELLA-SUBPROGRAM.                                                
010500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011001     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
011102     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011400*01 -COPY WMEDAREA                                                        
011500     SKIP3                                                                
011600*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
011700*   -COPY WDECAREA                                                        
011800     EJECT                                                                
011900 01  MESSAGE-CODES.                                                       
012000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012700     03  SUPPLIER-MISSING        PIC X(3)    VALUE '273'.                 
012800     03  SEGMENT-MISSING         PIC X(3)    VALUE '005'.                 
012900     03  GROUP-EXISTS            PIC X(3)    VALUE '274'.                 
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013400     SKIP3                                                                
013500*01 -COPY WMSGINIT                                                        
013601*                                                                         
013702                                                                          
013802 01  FILLER                      PIC X(24) VALUE 'WORKDAY-START '.        
013902*01  -COPY WORKAREA                                                       
014002*                                                                         
014102                                                                          
014202*    --- PARAMETRAR FÖR ABEND                                             
014302 01  ERROR-TEXT                  PIC X(80).                               
014402 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
014500     EJECT                                                                
014600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014700*                                                                         
014800 01  SPAR-AREA.                                                           
014900     03  SPAR-IDTRANS               PIC X(4)  VALUE '4431'.               
015000     03  SPAR-IDDISTR-ENTER         PIC S9(5) VALUE ZERO COMP-3.          
015100     03  SPAR-IDDISTR-NEXT          PIC S9(5) VALUE ZERO COMP-3.          
015200     03  SPAR-IDKUNDNR-ENTER        PIC S9(7) VALUE ZERO COMP-3.          
015300     03  SPAR-IDKUNDNR-NEXT         PIC S9(7) VALUE ZERO COMP-3.          
015400     03  SPAR-KDORDKL-ENTER         PIC S9    VALUE ZERO COMP-3.          
015500     03  SPAR-KDORDKL-NEXT          PIC S9    VALUE ZERO COMP-3.          
015600     03  SPAR-IDDIRLEV-ENTER        PIC X(5)  VALUE SPACE.                
015700     03  SPAR-IDDIRLEV-NEXT         PIC X(5)  VALUE SPACE.                
015800     EJECT                                                                
015900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016200     SKIP3                                                                
016300*01  MID -COPY W4I43101                                                   
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016600     SKIP3                                                                
016700*01  -COPY WMSGAREA                                                       
016800     EJECT                                                                
016900     03  MOD REDEFINES MSG-AREA.                                          
017000*      05  -COPY W4O43101                                                 
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017300     SKIP3                                                                
017400*01  -COPY WMFSAREA                                                       
017500     EJECT                                                                
017600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017700*                                                                         
017800     EJECT                                                                
017900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018000     SKIP3                                                                
018100 01  NYCKLAR-TILL-DLI.                                                    
018200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
018300                                                                          
018400     03  W-WDF118KY-X.                                                    
018500         05  W-IDDISTR         PIC S9(5)     VALUE ZERO COMP-3.           
018600         05  W-IDKUNDNR        PIC S9(7)     VALUE ZERO COMP-3.           
018700         05  W-KDORDKL         PIC S9        VALUE ZERO COMP-3.           
018800                                                                          
018900     03  W-WDF118KY-MIN-X.                                                
019000         05  W-IDDISTR-MIN     PIC S9(5)     VALUE ZERO COMP-3.           
019100         05  W-IDKUNDNR-MIN    PIC S9(7)     VALUE ZERO COMP-3.           
019200         05  W-KDORDKL-MIN     PIC S9        VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-WDF118KY-MAX-X.                                                
019500         05  W-IDDISTR-MAX     PIC S9(5)     VALUE 9999 COMP-3.           
019607         05  W-IDKUNDNR-MAX    PIC S9(7)    VALUE 999999 COMP-3.          
019700         05  W-KDORDKL-MAX     PIC S9        VALUE 9    COMP-3.           
019800                                                                          
019900     03  W-WDF118KY-MIN-X-L.                                              
019901         05  W-IDDISTR-MIN-L   PIC S9(5)     VALUE ZERO COMP-3.           
019902         05  FILLER            PIC X(5)      VALUE LOW-VALUE.             
019904                                                                          
019905     03  W-WDF118KY-MAX-X-H.                                              
019906         05  W-IDDISTR-MAX-H   PIC S9(5)     VALUE ZERO COMP-3.           
019908         05  FILLER            PIC X(5)      VALUE HIGH-VALUE.            
019909                                                                          
019910     03  W-WDF118KY-DUMMY-D.                                              
020004         05  W-IDDISTR-D       PIC S9(5)     VALUE 9999  COMP-3.          
020107         05  W-IDKUNDNR-D      PIC S9(7)     VALUE 999999 COMP-3.         
020204         05  W-KDORDKL-D       PIC S9        VALUE ZERO  COMP-3.          
020302                                                                          
020405     03  W-WDF118KY-DUMMY-C.                                              
020505         05  W-IDDISTR-C       PIC S9(5)     VALUE ZERO  COMP-3.          
020607         05  W-IDKUNDNR-C      PIC S9(7)     VALUE 999999 COMP-3.         
020705         05  W-KDORDKL-C       PIC S9        VALUE ZERO  COMP-3.          
020804                                                                          
020902     03   W-WDF6ASEQ-X.                                                   
021002         05  W-IDLEVNR-F6      PIC X(5)      VALUE SPACE.                 
021102         05  W-IDDISTR-F6      PIC S9(5)     VALUE ZERO COMP-3.           
021304         05  W-KDORDKL-F6      PIC S9        VALUE ZERO COMP-3.           
021305         05  W-IDKUNDNR-F6     PIC S9(7)     VALUE ZERO COMP-3.           
021404                                                                          
021405     03   W-WDF6ASEQ-MIN-X.                                               
021406         05  W-IDLEVNR-F6-MIN  PIC X(5)      VALUE SPACE.                 
021407         05  W-IDDISTR-F6-MIN  PIC S9(5)     VALUE ZERO COMP-3.           
021409         05  W-KDORDKL-F6-MIN  PIC S9        VALUE ZERO COMP-3.           
021410         05  FILLER            PIC X(4)      VALUE LOW-VALUE.             
021420                                                                          
021430     03   W-WDF6ASEQ-MAX-X.                                               
021440         05  W-IDLEVNR-F6-MAX  PIC X(5)      VALUE SPACE.                 
021450         05  W-IDDISTR-F6-MAX  PIC S9(5)     VALUE ZERO COMP-3.           
021460         05  W-KDORDKL-F6-MAX  PIC S9        VALUE ZERO COMP-3.           
021470         05  FILLER            PIC X(4)      VALUE HIGH-VALUE.            
021480                                                                          
021504     03  W-IDORDER-X.                                                     
021604         05  W-IDORDER         PIC S9(7)     VALUE ZERO COMP-3.           
021704                                                                          
021804     03  W-WDQ211KY-X.                                                    
021904         05  W-IDDC            PIC X(2)      VALUE SPACE.                 
022004         05  W-IDLEVNR         PIC X(5)      VALUE SPACE.                 
022104                                                                          
022105     03  W-IDGMT-X.                                                       
022106         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
022107         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
022108                                                                          
022204     03  W-IDDIRLEV-X             PIC  X(5)   VALUE SPACE.                
022304     03  W-IDDIRLEV-MAX-X         PIC  X(5)   VALUE '99999'.              
022404     03  W-IDDIRLEV-MIN-X         PIC  X(5)   VALUE SPACE.                
022504     03  W-IDDIRLEV-DUMMY         PIC  X(5)   VALUE '99999'.              
022604                                                                          
022704     SKIP2                                                                
022804*    --- STATUS-KOD FRÅN IMS                                              
022904 01  STATUS-WS                   PIC XX.                                  
023004     88  SEGMENT-FINNS                       VALUE '  '.                  
023104     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023204     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023304     88  BASEN-SLUT                          VALUE 'GB'.                  
023404     SKIP2                                                                
023504 01  GODK-STATUSKODER.                                                    
023604     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023704     SKIP3                                                                
023804 01  ALL-SSA.                                                             
023904     03 SSA1                     PIC X(64).                               
024004     03 SSA2                     PIC X(64).                               
024104     EJECT                                                                
024204*    --- IMS FUNKTIONSKODER                                               
024304*01  -COPY W0003                                                          
024404     EJECT                                                                
024504*    ---  DLI INPUT-OUTPUT AREA                                           
024604                                                                          
024704 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
024804 01  DLI-IO-WLLEVA01.                                                     
024904*    03  -COPY WDF101  -PRE LEVA-                                         
025004     EJECT                                                                
025104 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA18'.                    
025204 01  DLI-IO-WLLEVA18.                                                     
025304*    03  -COPY WDF118  -PRE LEVA-                                         
025404     EJECT                                                                
025504 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF601  '.                    
025604 01  DLI-IO-WDF601.                                                       
025704*    03  -COPY WDF601                                                     
025804 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ211  '.                    
025904 01  DLI-IO-WDQ211.                                                       
026004*    03  -COPY WDQ211                                                     
026104     EJECT                                                                
026105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201  '.                    
026106 01  DLI-IO-WDB201.                                                       
026107*    03  -COPY WDB201                                                     
026108     EJECT                                                                
026204 LINKAGE SECTION.                                                         
026304*01  -COPY W0009   -PRE MSG-                                              
026404*01  -COPY W0008   -PRE USEA-                                             
026504     05  FILLER                  PIC X.                                   
026604                                                                          
026704*01  -COPY W0008  -PRE LEVA-                                              
026804     05  FILLER                  PIC X.                                   
026904                                                                          
027004*01  -COPY W0008  -PRE WDF6-                                              
027104     05  FILLER                  PIC X.                                   
027204                                                                          
027304*01  -COPY W0008  -PRE WDQ2-                                              
027404     05  FILLER                  PIC X.                                   
027405                                                                          
027406*01  -COPY W0008  -PRE WDB2-                                              
027407     05  FILLER                  PIC X.                                   
027504     EJECT                                                                
027604 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB LEVA-PCB                      
027704                                   WDF6-PCB WDQ2-PCB WDB2-PCB.            
027804 MAIN SECTION.                                                            
027904     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LEVA-PCB                      
028004                                   WDF6-PCB WDQ2-PCB WDB2-PCB.            
028104                                                                          
028204     PERFORM IMS-GET-MSG                                                  
028304     IF SEGMENT-FINNS                                                     
028404       PERFORM A-INIT                                                     
028504       PERFORM B-KOLLA-NYCKLAR                                            
028604       IF NYCKLAR-OK                                                      
028704         IF MFS-UPDATE                                                    
028804           PERFORM G-KOLLA-INPUT                                          
028904           IF INDATA-OK                                                   
029004             PERFORM H-UPPDATERA                                          
029104           END-IF                                                         
029204         ELSE                                                             
029304           IF MFS-FIRST                                                   
029404             PERFORM C-FOERSTA-SIDA                                       
029504           ELSE                                                           
029604             IF MFS-NEXT                                                  
029704               PERFORM D-NAESTA-SIDA                                      
029804             ELSE                                                         
029904               PERFORM E-SAMMA-SIDA                                       
030004             END-IF                                                       
030104           END-IF                                                         
030204         END-IF                                                           
030304         IF ALLT-OK                                                       
030404           PERFORM F-LAES-VISA-INFO                                       
030504         END-IF                                                           
030604       END-IF                                                             
030704       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O43101 + 4                      
030804       PERFORM IMS-INSERT-MSG                                             
030904     END-IF                                                               
031004                                                                          
031104     MOVE ZERO TO RETURN-CODE                                             
031204     GOBACK                                                               
031304     .                                                                    
031404     EJECT                                                                
031504 A-INIT SECTION.                                                          
031604     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
031704                                                                          
031804     IF MSG-DUBBLA-TRANSKODER                                             
031904       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I43101                 
032004       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032104       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032204     ELSE                                                                 
032304       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I43101                  
032404       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032504       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032604     END-IF                                                               
032704                                                                          
032804     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032904     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033004     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033104                                                                          
033204     MOVE LOW-VALUE TO MSG-AREA                                           
033304     MOVE 'W4O431N1' TO MFS-IDMOD                                         
033404     MOVE '4431' TO MOD-IDTRANS                                           
033504     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
033604                             MOD-TEMFSINF                                 
033704                                                                          
033804     IF EGEN-MID OR HELP-MID                                              
033904       CONTINUE                                                           
034004     ELSE                                                                 
034104       MOVE SPACE TO MFS-KDTRTYP                                          
034204       MOVE '7' TO MFS-IDPFK                                              
034304     END-IF                                                               
034404                                                                          
034504     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
034604     .                                                                    
034704     EJECT                                                                
034804 B-KOLLA-NYCKLAR SECTION.                                                 
034904     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
035004                                                                          
035104     MOVE ALL '+'           TO MSGI-WMSGINIT                              
035204     MOVE '001'             TO MSGI-KDCALL                                
035304     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035404     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035504     MOVE '4431'            TO MSGI-IDTRANS                               
035604     IF EGEN-MID                                                          
035704       MOVE MID-IDDIRLEV-IN    TO MSGI-IDLEVNR                            
035804       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
035904     END-IF                                                               
036004     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036104     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
036204     MOVE JA TO NYCKLAR-SW                                                
036304     IF MSGI-IDLAND-SPR = 'GB'                                            
036404       MOVE 'GB' TO MED-IDSKYLT                                           
036504     ELSE                                                                 
036604       MOVE 'S' TO MED-IDSKYLT                                            
036704     END-IF                                                               
036804                                                                          
036904*    -- KONTROLL AV IDDIRLEV                                              
037004     IF GODK-MID                                                          
037104       MOVE MFS-RENSA-FAELT TO MOD-IDDIRLEV-IN                            
037204                                                                          
037304       IF MID-IDDIRLEV-IN NOT = ALL '+'                                   
037404         MOVE '7'         TO MFS-IDPFK                                    
037504         MOVE SPACE       TO MFS-KDTRTYP                                  
037604       END-IF                                                             
037704         MOVE MSGI-IDLEVNR  TO W-IDDIRLEV-X                               
037804                               W-IDDIRLEV-MIN-X                           
037904                                                                          
038004*    -- KONTROLL AV KDORDKL                                               
038104       MOVE SPACE           TO MOD-KDORDKL-IN                             
038204                                                                          
038304       IF MID-KDORDKL-IN NOT = ALL '+'                                    
038404         MOVE '7'         TO MFS-IDPFK                                    
038504         MOVE SPACE       TO MFS-KDTRTYP                                  
038604       END-IF                                                             
038704       IF MID-KDORDKL-IN = ALL '+'                                        
038804         MOVE MID-KDORDKL-UT TO MID-KDORDKL-IN                            
038904       END-IF                                                             
039004       INSPECT MID-KDORDKL-IN REPLACING LEADING SPACE BY ZERO             
039104       IF MID-KDORDKL-IN NUMERIC                                          
039204         MOVE MID-KDORDKL-IN TO W-KDORDKL                                 
039304                                W-KDORDKL-MIN                             
039404       END-IF                                                             
039504                                                                          
039604       IF MSGI-IDLEVNR   = ALL '+' AND                                    
039704          MID-KDORDKL-IN NOT = ALL '+'                                    
039804         MOVE NEJ TO NYCKLAR-SW                                           
039904       END-IF                                                             
040004                                                                          
040104*    -- KONTROLL AV IDDISTR                                               
040204       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                             
040304                                                                          
040404       IF MID-IDDISTR-IN NOT = ALL '+'                                    
040504         MOVE '7'         TO MFS-IDPFK                                    
040604         MOVE SPACE       TO MFS-KDTRTYP                                  
040704       END-IF                                                             
040804       INSPECT MSGI-IDDISTR  REPLACING LEADING SPACE BY ZERO              
040904       IF MSGI-IDDISTR    NUMERIC                                         
041004         MOVE MSGI-IDDISTR TO W-IDDISTR                                   
041104                              W-IDDISTR-MIN                               
041204       ELSE                                                               
041304         IF MSGI-IDLEVNR NOT = ALL '+'                                    
041404           MOVE NEJ TO NYCKLAR-SW                                         
041504         END-IF                                                           
041604       END-IF                                                             
041704       IF MSGI-IDLEVNR    = ALL '+' AND                                   
041804          MID-IDDISTR-IN NOT = ALL '+'                                    
041904         MOVE NEJ TO NYCKLAR-SW                                           
042004       END-IF                                                             
042104                                                                          
042204       IF GODK-MID OR NYCKLAR-OK                                          
042304         MOVE MSGI-IDLEVNR            TO MOD-IDDIRLEV-UT                  
042404         IF MID-KDORDKL-IN NOT = ALL '+'                                  
042504           MOVE MID-KDORDKL-IN        TO MOD-KDORDKL-UT                   
042604           INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE         
042704         ELSE                                                             
042804           MOVE SPACE                 TO MOD-KDORDKL-UT                   
042904         END-IF                                                           
043004         IF MSGI-IDDISTR  NOT = ALL '+'                                   
043104           MOVE MSGI-IDDISTR          TO MOD-IDDISTR-UT                   
043204           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
043304         ELSE                                                             
043404           MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-UT                   
043504         END-IF                                                           
043604                                                                          
043704       ELSE                                                               
043804         MOVE MFS-RENSA-FAELT TO MOD-IDDIRLEV-UT                          
043904         MOVE SPACE           TO MOD-KDORDKL-UT                           
044004         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
044104       END-IF                                                             
044204                                                                          
044304       IF NYCKLAR-FEL                                                     
044404         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
044504         CALL WMEDKONV USING MED-WMEDAREA                                 
044604         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
044704         PERFORM MFS-RENSA-FAELT-IN                                       
044804         PERFORM MFS-RENSA-FAELT-UT                                       
044904       END-IF                                                             
045004     ELSE                                                                 
045104       MOVE NEJ TO NYCKLAR-SW                                             
045204       PERFORM MFS-RENSA-FAELT-IN                                         
045304       PERFORM MFS-RENSA-FAELT-UT                                         
045404     END-IF                                                               
045504     .                                                                    
045604     EJECT                                                                
045704 C-FOERSTA-SIDA SECTION.                                                  
045804     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
045904                                                                          
046004     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
046104     CALL WMEDKONV USING MED-WMEDAREA                                     
046204     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
046304                                                                          
046404     PERFORM MFS-RENSA-FAELT-IN                                           
046504     .                                                                    
046604     EJECT                                                                
046704 D-NAESTA-SIDA SECTION.                                                   
046804     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
046904                                                                          
047004     IF SPAR-IDTRANS = '4431'                                             
047104       MOVE SPAR-IDDISTR-NEXT TO W-IDDISTR-MIN                            
047204                                 W-IDDISTR                                
047304       MOVE SPAR-IDKUNDNR-NEXT TO W-IDKUNDNR-MIN                          
047404                                 W-IDKUNDNR                               
047504       MOVE SPAR-KDORDKL-NEXT TO W-KDORDKL-MIN                            
047604                                 W-KDORDKL                                
047704     PERFORM MFS-RENSA-FAELT-IN                                           
047804     ELSE                                                                 
047904       PERFORM MFS-RENSA-FAELT-IN                                         
048004     END-IF                                                               
048104     .                                                                    
048204     EJECT                                                                
048304 E-SAMMA-SIDA SECTION.                                                    
048404     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
048504                                                                          
048604     IF SPAR-IDTRANS = '4431' OR '0551'                                   
048704       MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-MIN                          
048804                                   W-IDDISTR                              
048904       MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR-MIN                         
049004                                   W-IDKUNDNR                             
049104       MOVE SPAR-IDDIRLEV-ENTER TO W-IDDIRLEV-MIN-X                       
049204                                   W-IDDIRLEV-X                           
049304       MOVE SPAR-KDORDKL-ENTER  TO W-KDORDKL-MIN                          
049404                                   W-KDORDKL                              
049504       IF MID-CMD           = ALL '+' AND                                 
049604          MID-KDORDKL-E     = ALL '+' AND                                 
049704          MID-IDDISTR-E     = ALL '+' AND                                 
049804          MID-IDKUNDNR-E    = ALL '+' AND                                 
049904          MID-IDDC-E        = ALL '+' AND                                 
050004          MID-KDVIA-E       = ALL '+' AND                                 
050104          MID-TIMINUT-CUT-E = ALL '+' AND                                 
050204          MID-KVDAGAR-LEV-E = ALL '+' AND                                 
050304          MID-KVDAGAR-TPO-E = ALL '+' AND                                 
050404          MID-KVDAGAR-DIFF-E = ALL '+'                                    
050504         PERFORM MFS-RENSA-FAELT-IN                                       
050604       ELSE                                                               
050704         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
050804         CALL WMEDKONV USING MED-WMEDAREA                                 
050904         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
051004         PERFORM MFS-ROER-EJ-FAELT-IN                                     
051104         PERFORM MFS-LAES-IN-IGEN                                         
051204       END-IF                                                             
051304     ELSE                                                                 
051404       PERFORM MFS-RENSA-FAELT-IN                                         
051504     END-IF                                                               
051604     .                                                                    
051704     EJECT                                                                
051804 F-LAES-VISA-INFO SECTION.                                                
051904     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
052004                                                                          
052104     PERFORM FA-LAES-GRUNDDATA                                            
052204                                                                          
052304     IF SEGMENT-SAKNAS                                                    
052404        MOVE SUPPLIER-MISSING TO MED-IDMFSFEL                             
052504        CALL WMEDKONV USING MED-WMEDAREA                                  
052604        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
052704        PERFORM MFS-RENSA-FAELT-UT                                        
052804     ELSE                                                                 
052904       MOVE LEVA-LEV-IDLEVNR TO SPAR-IDDIRLEV-ENTER                       
053004                                                                          
053104       MOVE +1 TO INDX                                                    
053204       IF MFS-UPDATE                                                      
053304         MOVE SPAR-IDDISTR-ENTER TO W-IDDISTR-MIN                         
053404         MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR-MIN                       
053504         MOVE SPAR-KDORDKL-ENTER TO W-KDORDKL-MIN                         
053604       END-IF                                                             
053704       PERFORM FB-LAES-RADDATA                                            
053804       IF SEGMENT-FINNS                                                   
053904         MOVE LEVA-DSTY-IDDISTR TO SPAR-IDDISTR-ENTER                     
054004         MOVE LEVA-DSTY-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                   
054104         MOVE LEVA-DSTY-KDORDKL TO SPAR-KDORDKL-ENTER                     
054204         MOVE LEVA-DSTY-IDDISTR TO SPAR-IDDISTR-NEXT                      
054304         MOVE LEVA-DSTY-IDKUNDNR TO SPAR-IDKUNDNR-NEXT                    
054404         MOVE LEVA-DSTY-KDORDKL TO SPAR-KDORDKL-NEXT                      
054504         MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-MIN                        
054604         MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR-MIN                       
054704         MOVE SPAR-KDORDKL-ENTER  TO W-KDORDKL-MIN                        
054804       ELSE                                                               
054904         MOVE W-IDDISTR-MIN     TO SPAR-IDDISTR-ENTER                     
055004         MOVE W-IDDISTR-MIN     TO SPAR-IDDISTR-NEXT                      
055104         MOVE W-IDKUNDNR-MIN    TO SPAR-IDKUNDNR-ENTER                    
055204         MOVE W-IDKUNDNR-MIN    TO SPAR-IDKUNDNR-NEXT                     
055304         MOVE W-KDORDKL-MIN     TO SPAR-KDORDKL-ENTER                     
055404         MOVE W-KDORDKL-MIN     TO SPAR-KDORDKL-NEXT                      
055504         MOVE SEGMENT-MISSING TO MED-IDMFSFEL                             
055604         CALL WMEDKONV USING MED-WMEDAREA                                 
055704         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
055804         PERFORM MFS-RENSA-FAELT-UT                                       
055904       END-IF                                                             
056004                                                                          
056104       PERFORM UNTIL INDX > MAX-INDX                                      
056204         IF SEGMENT-FINNS                                                 
056304           MOVE JA  TO VISA-SW                                            
056404           MOVE LEVA-DSTY-KDORDKL      TO MOD-KDORDKL (INDX)              
056504           MOVE LEVA-DSTY-IDDISTR      TO MOD-IDDISTR (INDX)              
056604           MOVE LEVA-DSTY-IDKUNDNR     TO MOD-IDKUNDNR (INDX)             
056704           MOVE LEVA-DSTY-IDDC         TO MOD-IDDC (INDX)                 
056804           MOVE LEVA-DSTY-KDVIA        TO MOD-KDVIA (INDX)                
056904                                                                          
057004           MOVE LEVA-DSTY-TIMINUT-CUT  TO WS-TIMINUT                      
057104           MOVE WS-TIMINUT(2:2)        TO W-TIMINUT (1:2)                 
057204           MOVE WS-TIMINUT(4:2)        TO W-TIMINUT (3:2)                 
057304           MOVE W-TIMINUT              TO MOD-TIMINUT (INDX)              
057404           MOVE LEVA-DSTY-KVDAGAR-LEV  TO MOD-KVDAGAR-LEV (INDX)          
057504           MOVE LEVA-DSTY-KVDAGAR-TPO  TO MOD-KVDAGAR-TPO (INDX)          
057604           MOVE LEVA-DSTY-KVDAGAR-DIFF TO MOD-KVDAGAR-DIFF(INDX)          
057704         ELSE                                                             
057804           MOVE NEJ TO VISA-SW                                            
057904           PERFORM UNTIL INDX > MAX-INDX                                  
058004           MOVE MFS-RENSA-FAELT TO MOD-KDORDKL (INDX)                     
058104                                   MOD-IDDISTR (INDX)                     
058204                                   MOD-IDKUNDNR (INDX)                    
058304                                   MOD-IDDC (INDX)                        
058404                                   MOD-KDVIA (INDX)                       
058504                                   MOD-TIMINUT     (INDX)                 
058604                                   MOD-KVDAGAR-LEV (INDX)                 
058704                                   MOD-KVDAGAR-TPO (INDX)                 
058804                                   MOD-KVDAGAR-DIFF(INDX)                 
058904           ADD +1 TO INDX                                                 
059004           END-PERFORM                                                    
059104         END-IF                                                           
059204         IF VISA-OK                                                       
059304           ADD 1 TO INDX                                                  
059404           PERFORM FB-LAES-RADDATA                                        
059504         END-IF                                                           
059604       END-PERFORM                                                        
059704                                                                          
059804       IF SEGMENT-FINNS                                                   
059904         MOVE LEVA-DSTY-IDDISTR TO SPAR-IDDISTR-NEXT                      
060004         MOVE LEVA-DSTY-IDKUNDNR TO SPAR-IDKUNDNR-NEXT                    
060104         MOVE LEVA-DSTY-KDORDKL TO SPAR-KDORDKL-NEXT                      
060204         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
060304         CALL WMEDKONV USING MED-WMEDAREA                                 
060404         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
060504       END-IF                                                             
060604                                                                          
060704       MOVE '002'      TO MSGI-KDCALL                                     
060804       MOVE '4431'   TO SPAR-IDTRANS                                      
060904       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
061004       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
061104     END-IF                                                               
061204     .                                                                    
061304     EJECT                                                                
061404 FA-LAES-GRUNDDATA SECTION.                                               
061504     MOVE 'FA-LAES-GRUNDDAT' TO CURRENT-SECTION                           
061604                                                                          
061704     PERFORM IMS-GET-LEVA                                                 
061804     .                                                                    
061904     EJECT                                                                
062004 FB-LAES-RADDATA SECTION.                                                 
062104     MOVE 'FB-LAES-RADDATA ' TO CURRENT-SECTION                           
062204                                                                          
062304     PERFORM IMS-GET-LEVA-DSTY                                            
062404     .                                                                    
062504     EJECT                                                                
062604 G-KOLLA-INPUT SECTION.                                                   
062704     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
062804                                                                          
062904     MOVE JA  TO INDATA-SW                                                
063004     IF MID-CMD = ALL '+'       AND MID-KDORDKL-E = ALL '+' AND           
063104        MID-IDDISTR-E = ALL '+' AND MID-IDDC-E              AND           
063204        MID-KDVIA-E = ALL '+'   AND MID-TIMINUT-CUT-E = ALL '+'           
063304        AND MID-IDKUNDNR-E    = ALL '+'                                   
063404        AND MID-KVDAGAR-LEV-E = ALL '+'                                   
063504        AND MID-KVDAGAR-TPO-E = ALL '+'                                   
063604        AND MID-TECKEN = '+' AND MID-KVDAGAR-DIFF-E = ALL '+'             
063706        AND MID-IDKUNDNR-E = '+'                                          
063806       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
063906       CALL WMEDKONV USING MED-WMEDAREA                                   
064006       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
064106       PERFORM MFS-ROER-EJ-FAELT-IN                                       
064206       PERFORM MFS-ROER-EJ-FAELT-UT                                       
064306       MOVE NEJ TO INDATA-SW                                              
064406     ELSE                                                                 
064506       IF MID-CMD = 'N' OR 'E' OR 'D'                                     
064507          IF (MID-KDORDKL-E      = ALL '+' AND                            
064508              MID-IDDISTR-E      = ALL '+' AND                            
064509              MID-IDKUNDNR-E     = ALL '+' AND                            
064510              MID-IDDC-E         = ALL '+' AND                            
064520              MID-KDVIA-E        = ALL '+' AND                            
064530              MID-TIMINUT-CUT-E  = ALL '+' AND                            
064540              MID-KVDAGAR-LEV-E  = ALL '+' AND                            
064550              MID-KVDAGAR-TPO-E  = ALL '+' AND                            
064560              MID-TECKEN         = ALL '+' AND                            
064570              MID-KVDAGAR-DIFF-E = ALL '+')                               
064580            MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR                       
064590            MOVE NEJ TO INDATA-SW                                         
064600          ELSE                                                            
064601            MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR                     
064602          END-IF                                                          
064706       ELSE                                                               
064806          MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR                         
064906          MOVE NEJ TO INDATA-SW                                           
065006       END-IF                                                             
065106                                                                          
065206       IF MID-KDORDKL-E NOT = ALL '+'                                     
065306         IF MID-KDORDKL-E NOT NUMERIC                                     
065406                                                                          
065506           MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-E-ATTR                   
065606           MOVE NEJ TO INDATA-SW                                          
065706         ELSE                                                             
065806           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDKL-E-ATTR                 
065906         END-IF                                                           
066006       END-IF                                                             
066106       IF MID-IDDISTR-E NOT = ALL '+'                                     
066206       INSPECT MID-IDDISTR-E REPLACING LEADING SPACE BY ZERO              
066306         IF MID-IDDISTR-E NOT NUMERIC                                     
066406           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-E-ATTR                   
066506           MOVE NEJ TO INDATA-SW                                          
066606         ELSE                                                             
066706           MOVE MID-IDDISTR-E       TO TEST-IDDISTR                       
066806           IF DIST03-SVERIGE                                              
066906             IF MID-IDDC-E = '11' OR 'SE'                                 
067006               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-E-ATTR             
067106             ELSE                                                         
067206               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-E-ATTR               
067306               MOVE NEJ TO INDATA-SW                                      
067406             END-IF                                                       
067506           ELSE                                                           
067606             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-E-ATTR               
067706           END-IF                                                         
067806         END-IF                                                           
067807         IF MID-IDKUNDNR-E = ALL '+'                                      
067808            MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-E-ATTR                 
067809            MOVE NEJ TO INDATA-SW                                         
067810         ELSE                                                             
067812            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR               
067820         END-IF                                                           
067906       END-IF                                                             
068006       IF MID-IDKUNDNR-E NOT = ALL '+'                                    
068106       INSPECT MID-IDKUNDNR-E REPLACING LEADING SPACE BY ZERO             
068206         IF MID-IDKUNDNR-E NOT NUMERIC                                    
068306           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-E-ATTR                  
068406           MOVE NEJ TO INDATA-SW                                          
068506         ELSE                                                             
068507           IF MID-IDDISTR-E   = ALL '9999'                                
068508             IF MID-IDKUNDNR-E NOT = '999999'                             
068509               MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-E-ATTR              
068511               MOVE NEJ TO INDATA-SW                                      
068520             ELSE                                                         
068530               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR            
068540             END-IF                                                       
068550           ELSE                                                           
068551              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR             
068552             IF INDATA-OK                                                 
068553               MOVE MID-IDDISTR-E        TO W-IDDISTR-WDB2                
068554               MOVE MID-IDKUNDNR-E       TO W-IDKUNDNR-WDB2               
068555               PERFORM IMS-GU-WDB201                                      
068556               IF SEGMENT-FINNS OR MID-IDKUNDNR-E = '999999'              
068557                  MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-E-ATTR         
068559               ELSE                                                       
068560                  MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-E-ATTR         
068561                  MOVE NEJ TO INDATA-SW                                   
068563               END-IF                                                     
068564             END-IF                                                       
068570           END-IF                                                         
069104         END-IF                                                           
069204       END-IF                                                             
069304       MOVE MID-IDDC-E    TO WS-IDDC                                      
069404       IF MID-IDDC-E NOT = ALL '+'                                        
069504         IF NOT GOOD-DDC AND                                              
069604            NOT CDC-SE                                                    
069704           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-E-ATTR                     
069804           MOVE NEJ TO INDATA-SW                                          
069904         ELSE                                                             
070004           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-E-ATTR                   
070104         END-IF                                                           
070204       END-IF                                                             
070304       IF MID-KDVIA-E NOT = ALL '+'                                       
070404         INSPECT MID-KDVIA-E REPLACING LEADING SPACE BY ZERO              
070504         IF MID-KDVIA-E NOT NUMERIC                                       
070604           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDVIA-E-ATTR                 
070704           MOVE NEJ TO INDATA-SW                                          
070804         ELSE                                                             
070904           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDVIA-E-ATTR                 
071004         END-IF                                                           
071104       END-IF                                                             
071204       IF MID-TIMINUT-CUT-E NOT = ALL '+'                                 
071304         MOVE MID-TIMINUT-CUT-E TO DEC-IDFRIDATA                          
071404         PERFORM S01-FORMATERING-MED-RDECDATA                             
071504         IF DEC-KDSVAR-OK                                                 
071604          MOVE DEC-IDEDITDATA TO W-TIMINUT                                
071704          MOVE W-TIMINUT(1:2) TO WS-TIMINUT(2:2)                          
071804          MOVE W-TIMINUT(3:2) TO WS-TIMINUT(4:2)                          
071904          MOVE WS-TIMINUT     TO TID                                      
072004          IF TIMME-OK AND MINUT-OK                                        
072104           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TIMINUT-CUT-E-ATTR          
072204          ELSE                                                            
072304           MOVE MFS-ALFA-FAELT-FEL     TO MOD-TIMINUT-CUT-E-ATTR          
072404           MOVE NEJ TO INDATA-SW                                          
072504          END-IF                                                          
072604         ELSE                                                             
072704          MOVE MFS-ALFA-FAELT-FEL      TO MOD-TIMINUT-CUT-E-ATTR          
072804          MOVE NEJ TO INDATA-SW                                           
072904         END-IF                                                           
073004       END-IF                                                             
073104       IF MID-KVDAGAR-LEV-E NOT = ALL '+'                                 
073204         INSPECT MID-KVDAGAR-LEV-E REPLACING LEADING SPACE BY ZERO        
073304         IF MID-KVDAGAR-LEV-E  NOT NUMERIC                                
073404           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVDAGAR-LEV-E-ATTR           
073504           MOVE NEJ TO INDATA-SW                                          
073604         ELSE                                                             
073704           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-LEV-E-ATTR             
073804         END-IF                                                           
073904       END-IF                                                             
074004       IF MID-KVDAGAR-TPO-E NOT = ALL '+'                                 
074104         INSPECT MID-KVDAGAR-TPO-E REPLACING LEADING SPACE BY ZERO        
074204         IF MID-KVDAGAR-TPO-E  NOT NUMERIC                                
074304           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVDAGAR-TPO-E-ATTR           
074404           MOVE NEJ TO INDATA-SW                                          
074504         ELSE                                                             
074604           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-TPO-E-ATTR             
074704         END-IF                                                           
074804       END-IF                                                             
074904       IF MID-TECKEN = '+' OR '-'                                         
075004         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TECKEN-ATTR                   
075104       ELSE                                                               
075204         MOVE MFS-ALFA-FAELT-FEL     TO MOD-TECKEN-ATTR                   
075304         MOVE NEJ TO INDATA-SW                                            
075404       END-IF                                                             
075504       IF MID-KVDAGAR-DIFF-E NOT = ALL '+'                                
075604        INSPECT MID-KVDAGAR-DIFF-E REPLACING LEADING SPACE BY ZERO        
075704         IF MID-KVDAGAR-DIFF-E  NOT NUMERIC                               
075804           MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-DIFF-E-ATTR              
075904           MOVE NEJ TO INDATA-SW                                          
076004         ELSE                                                             
076104           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-DIFF-E-ATTR            
076204         END-IF                                                           
076304       END-IF                                                             
076404       IF MID-CMD = 'D'                                                   
076604         IF MID-IDDISTR-E = 9999                                          
076704           MOVE NEJ TO INDATA-SW                                          
076804           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-E-ATTR                   
076904         ELSE                                                             
076905          IF MID-IDKUNDNR-E = '999999'                                    
076906             PERFORM IMS-GET-LEVA                                         
076907             MOVE MID-IDDISTR-E TO W-IDDISTR-MIN-L                        
076908                                   W-IDDISTR-MAX-H                        
076909             PERFORM IMS-GNP-LEVA-DSTY                                    
076910             MOVE JA TO DELETE-SW                                         
076911             PERFORM UNTIL SEGMENT-SAKNAS OR NOT DELETE-OK                
076912               IF LEVA-DSTY-IDKUNDNR NOT = 999999 AND                     
076913                  LEVA-DSTY-KDORDKL  = MID-KDORDKL-E                      
076918                  MOVE NEJ TO DELETE-SW                                   
076919               END-IF                                                     
076920               PERFORM IMS-GNP-LEVA-DSTY                                  
076921             END-PERFORM                                                  
076922             IF DELETE-OK                                                 
076923               MOVE MFS-NUM-FAELT-RAETT TO MOD-CMD-ATTR                   
076924             ELSE                                                         
076925               MOVE MFS-NUM-FAELT-FEL TO MOD-CMD-ATTR                     
076926               MOVE NEJ TO INDATA-SW                                      
076927             END-IF                                                       
076930          END-IF                                                          
077404         END-IF                                                           
077504       END-IF                                                             
077604                                                                          
077704       IF INDATA-FEL                                                      
077804         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
077904         CALL WMEDKONV USING MED-WMEDAREA                                 
078004         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
078104         PERFORM MFS-ROER-EJ-FAELT-UT                                     
078204         PERFORM MFS-ROER-EJ-FAELT-IN                                     
078304       ELSE                                                               
078404*CHECK IF THERE IS A DUMMY RECORD, IT MUST EXIST                          
078604         MOVE MID-KDORDKL-E     TO W-KDORDKL-D                            
078704         PERFORM IMS-GET-DUMMY-DISTR                                      
078804         IF SEGMENT-SAKNAS                                                
078904          IF MID-CMD = 'N' AND                                            
079104             MID-IDDISTR-E      = 9999                                    
079204            CONTINUE                                                      
079304           ELSE                                                           
079404            MOVE 'DEFAULT MISSING' TO MOD-TEMFSFEL                        
079504            MOVE NEJ TO INDATA-SW                                         
079604            MOVE NEJ TO ALLT-OK-SW                                        
079704            MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-E-ATTR                  
079804            PERFORM MFS-ROER-EJ-FAELT-IN                                  
079904            PERFORM MFS-ROER-EJ-FAELT-UT                                  
080004           END-IF                                                         
080104         ELSE                                                             
080204           MOVE MID-IDDISTR-E  TO W-IDDISTR                               
080304           MOVE MID-IDKUNDNR-E TO W-IDKUNDNR                              
080404           MOVE MID-KDORDKL-E  TO W-KDORDKL                               
080504           PERFORM IMS-GET-LEVA                                           
080604           IF MID-CMD = 'N'                                               
080704             PERFORM IMS-GU-LEVA-DSTY                                     
080804             IF SEGMENT-FINNS                                             
080904               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-E-ATTR               
081004               MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-E-ATTR               
081104               MOVE GROUP-EXISTS      TO MED-IDMFSFEL                     
081204               MOVE NEJ               TO INDATA-SW                        
081304               MOVE NEJ               TO ALLT-OK-SW                       
081404             ELSE                                                         
081607               IF MID-IDKUNDNR-E = 999999                                 
081704                  CONTINUE                                                
081804               ELSE                                                       
081905                  MOVE MID-IDDISTR-E  TO W-IDDISTR-C                      
082005                  MOVE MID-KDORDKL-E  TO W-KDORDKL-C                      
082106*CHECK IF THERE IS A DUMMY RECORD, IT MUST EXIST                          
082204                  PERFORM IMS-GET-DUMMY-CUST                              
082304                  IF SEGMENT-SAKNAS                                       
082305                    MOVE 999 TO MED-IDMFSFEL                              
082404                    MOVE 'DEFAULT MISSING' TO MOD-TEMFSFEL                
082504                    MOVE NEJ TO INDATA-SW                                 
082604                    MOVE NEJ TO ALLT-OK-SW                                
082704                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-E-ATTR         
082804                    PERFORM MFS-ROER-EJ-FAELT-IN                          
082904                    PERFORM MFS-ROER-EJ-FAELT-UT                          
083004                  END-IF                                                  
083104               END-IF                                                     
083204             END-IF                                                       
083304           END-IF                                                         
083404           IF MID-CMD = 'D'                                               
083504             PERFORM IMS-GET-LEVA-DSTY                                    
083604             IF SEGMENT-SAKNAS                                            
083704               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-E-ATTR            
083904               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
084004               MOVE NEJ                  TO INDATA-SW                     
084104             END-IF                                                       
084204           END-IF                                                         
084304           IF MID-CMD = 'E'                                               
084404             PERFORM IMS-GET-LEVA-DSTY                                    
084504             IF SEGMENT-SAKNAS                                            
084604               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-E-ATTR            
084705               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKUNDNR-E-ATTR           
084805               MOVE MFS-NUM-FAELT-FEL    TO MOD-KDORDKL-E-ATTR            
084905               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
085005               MOVE NEJ                  TO INDATA-SW                     
085105             END-IF                                                       
085205           END-IF                                                         
085305                                                                          
085405           IF INDATA-FEL AND MED-IDMFSFEL NOT = 999                       
085505            IF MED-IDMFSFEL = SPACE                                       
085605             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
085705            END-IF                                                        
085805             CALL WMEDKONV USING MED-WMEDAREA                             
085905             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
086005             PERFORM MFS-ROER-EJ-FAELT-UT                                 
086105             PERFORM MFS-ROER-EJ-FAELT-IN                                 
086205           END-IF                                                         
086305         END-IF                                                           
086405       END-IF                                                             
086505     END-IF                                                               
086605     .                                                                    
086705     EJECT                                                                
086805 H-UPPDATERA SECTION.                                                     
086905     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
087005                                                                          
087105     PERFORM IMS-GET-LEVA                                                 
087205     IF SEGMENT-FINNS                                                     
087305*    MOVE MID-KDORDKL-E TO W-KDORDKL-MIN                                  
087405*    MOVE MID-IDDISTR-E TO W-IDDISTR-MIN                                  
087505*    MOVE MID-KDORDKL-E TO W-KDORDKL                                      
087605*    MOVE MID-IDDISTR-E TO W-IDDISTR                                      
087705      PERFORM IMS-GU-LEVA-DSTY                                            
087805       IF SEGMENT-FINNS                                                   
087905         IF MID-CMD = 'D'                                                 
088005           PERFORM IMS-DLET-LEVA-DSTY                                     
088105         ELSE                                                             
088205           IF MID-CMD = 'E'                                               
088305             IF MID-KDORDKL-E NOT = ALL '+'                               
088405*???           MOVE MID-KDORDKL-E     TO LEVA-DSTY-IDDISTR                
088505               MOVE MID-KDORDKL-E     TO LEVA-DSTY-KDORDKL                
088605             END-IF                                                       
088705             IF MID-IDDISTR-E NOT = ALL '+'                               
088805               MOVE MID-IDDISTR-E     TO LEVA-DSTY-IDDISTR                
088905             END-IF                                                       
089005             IF MID-IDKUNDNR-E NOT = ALL '+'                              
089105               MOVE MID-IDKUNDNR-E    TO LEVA-DSTY-IDKUNDNR               
089205             END-IF                                                       
089305             IF MID-IDDC-E NOT = ALL '+'                                  
089405               MOVE MID-IDDC-E        TO LEVA-DSTY-IDDC                   
089505             END-IF                                                       
089605             IF MID-KDVIA-E NOT = ALL '+'                                 
089705               MOVE MID-KDVIA-E       TO LEVA-DSTY-KDVIA                  
089805             END-IF                                                       
089905             IF MID-TIMINUT-CUT-E NOT = ALL '+'                           
090005               MOVE WS-TIMINUT           TO LEVA-DSTY-TIMINUT-CUT         
090105             END-IF                                                       
090205             IF MID-KVDAGAR-LEV-E NOT = ALL '+'                           
090305               MOVE MID-KVDAGAR-LEV-E TO LEVA-DSTY-KVDAGAR-LEV            
090405             END-IF                                                       
090505             IF MID-KVDAGAR-TPO-E NOT = ALL '+'                           
090605               MOVE MID-KVDAGAR-TPO-E TO LEVA-DSTY-KVDAGAR-TPO            
090705             END-IF                                                       
090805             IF MID-KVDAGAR-DIFF-E NOT = ALL '+'                          
090905               IF MID-TECKEN = '-'                                        
091005                 COMPUTE LEVA-DSTY-KVDAGAR-DIFF =                         
091105                         MID-KVDAGAR-DIFF-E  * -1                         
091205               ELSE                                                       
091305                 MOVE MID-KVDAGAR-DIFF-E TO LEVA-DSTY-KVDAGAR-DIFF        
091405               END-IF                                                     
091505             END-IF                                                       
091605                                                                          
091705             PERFORM IMS-REPL-LEVA-DSTY                                   
091805             PERFORM HA-UPPDATERA-FORDROJD-DDGS                           
091905           END-IF                                                         
092005         END-IF                                                           
092105       ELSE                                                               
092205         IF MID-CMD = 'N'                                                 
092305          IF MID-KDORDKL-E NOT = ALL '+'                                  
092405           MOVE MID-KDORDKL-E      TO LEVA-DSTY-KDORDKL                   
092505          ELSE                                                            
092605           MOVE ZERO TO LEVA-DSTY-KDORDKL                                 
092705          END-IF                                                          
092805          IF MID-IDDISTR-E NOT = ALL '+'                                  
092905           MOVE MID-IDDISTR-E      TO LEVA-DSTY-IDDISTR                   
093005          ELSE                                                            
093105           MOVE ZERO               TO LEVA-DSTY-IDDISTR                   
093205          END-IF                                                          
093305          IF MID-IDKUNDNR-E NOT = ALL '+'                                 
093405           MOVE MID-IDKUNDNR-E     TO LEVA-DSTY-IDKUNDNR                  
093505          ELSE                                                            
093605           MOVE ZERO               TO LEVA-DSTY-IDKUNDNR                  
093705          END-IF                                                          
093805          IF MID-IDDC-E NOT = ALL '+'                                     
093905           MOVE MID-IDDC-E         TO LEVA-DSTY-IDDC                      
094005          ELSE                                                            
094105           MOVE SPACE              TO LEVA-DSTY-IDDC                      
094205          END-IF                                                          
094305          IF MID-KDVIA-E NOT = ALL '+'                                    
094405           MOVE MID-KDVIA-E        TO LEVA-DSTY-KDVIA                     
094505          ELSE                                                            
094605           MOVE ZERO               TO LEVA-DSTY-KDVIA                     
094705          END-IF                                                          
094805          IF MID-TIMINUT-CUT-E NOT = ALL '+'                              
094905            MOVE WS-TIMINUT        TO LEVA-DSTY-TIMINUT-CUT               
095005          ELSE                                                            
095105            MOVE ZERO              TO LEVA-DSTY-TIMINUT-CUT               
095205          END-IF                                                          
095305          IF MID-KVDAGAR-LEV-E NOT = ALL '+'                              
095405           MOVE MID-KVDAGAR-LEV-E  TO LEVA-DSTY-KVDAGAR-LEV               
095505          ELSE                                                            
095605           MOVE ZERO               TO LEVA-DSTY-KVDAGAR-LEV               
095705          END-IF                                                          
095805          IF MID-KVDAGAR-TPO-E NOT = ALL '+'                              
095905           MOVE MID-KVDAGAR-TPO-E  TO LEVA-DSTY-KVDAGAR-TPO               
096005          ELSE                                                            
096105           MOVE ZERO               TO LEVA-DSTY-KVDAGAR-TPO               
096205          END-IF                                                          
096305          IF MID-KVDAGAR-DIFF-E NOT = ALL '+'                             
096405            IF MID-TECKEN = '-'                                           
096505               COMPUTE LEVA-DSTY-KVDAGAR-DIFF =                           
096605                       MID-KVDAGAR-DIFF-E  * -1                           
096705            ELSE                                                          
096805             MOVE MID-KVDAGAR-DIFF-E TO LEVA-DSTY-KVDAGAR-DIFF            
096905            END-IF                                                        
097005          ELSE                                                            
097105           MOVE ZERO                 TO LEVA-DSTY-KVDAGAR-DIFF            
097205          END-IF                                                          
097305          PERFORM IMS-ISRT-LEVA-DSTY                                      
097405          IF SEGMENT-FINNS-REDAN                                          
097505             MOVE NEJ TO ALLT-OK-SW                                       
097605             MOVE GROUP-EXISTS     TO MED-IDMFSFEL                        
097705             CALL WMEDKONV USING MED-WMEDAREA                             
097805             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
097905             PERFORM MFS-ROER-EJ-FAELT-UT                                 
098005*            PERFORM MFS-RENSA-FAELT-IN                                   
098105          ELSE                                                            
098205             PERFORM HA-UPPDATERA-FORDROJD-DDGS                           
098305          END-IF                                                          
098405         ELSE                                                             
098505           MOVE NEJ TO ALLT-OK-SW                                         
098605           MOVE SEGMENT-MISSING  TO MED-IDMFSFEL                          
098705           CALL WMEDKONV USING MED-WMEDAREA                               
098805           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
098905           PERFORM MFS-RENSA-FAELT-UT                                     
099005         END-IF                                                           
099105       END-IF                                                             
099205       IF ALLT-OK                                                         
099305         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
099405         CALL WMEDKONV USING MED-WMEDAREA                                 
099505         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
099605         PERFORM MFS-FORM-ATTR                                            
099705         PERFORM MFS-RENSA-FAELT-IN                                       
099805       END-IF                                                             
099905     ELSE                                                                 
100005      MOVE SUPPLIER-MISSING TO MED-IDMFSFEL                               
100105      CALL WMEDKONV USING MED-WMEDAREA                                    
100205      MOVE MED-MFSFEL TO MOD-TEMFSFEL                                     
100305      PERFORM MFS-RENSA-FAELT-UT                                          
100405     END-IF                                                               
100505     .                                                                    
100605     EJECT                                                                
100705 HA-UPPDATERA-FORDROJD-DDGS   SECTION.                                    
100805     MOVE 'HA-UPD-DDGS     ' TO CURRENT-SECTION                           
100905                                                                          
100906*    *UPDATE ORDERS NOT YET PRINTED WITH NEW SHIPPING DATE.               
100908                                                                          
101408     IF LEVA-DSTY-IDDISTR NOT = 9999                                      
101409        IF LEVA-DSTY-IDKUNDNR NOT = 999999                                
101608          MOVE LEVA-LEV-IDLEVNR   TO W-IDLEVNR-F6                         
101708          MOVE LEVA-DSTY-IDDISTR  TO W-IDDISTR-F6                         
101908          MOVE LEVA-DSTY-KDORDKL  TO W-KDORDKL-F6                         
101909          MOVE LEVA-DSTY-IDKUNDNR TO W-IDKUNDNR-F6                        
101915                                                                          
101916          MOVE LEVA-DSTY-KVDAGAR-DIFF     TO WS-DSTY-KVDAGAR-DIFF         
101920          MOVE LEVA-DSTY-KVDAGAR-TPO                                      
101930                                    TO WS-DSTY-KVDAGAR-TPO                
102008                                                                          
102108          PERFORM IMS-GHN-WDF601                                          
102208          PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                      
102308             IF PUDH-TITPO > ZERO                                         
102408             OR PUDH-TIREPDAT > ZERO                                      
102508                PERFORM HAA-UPPDATERA-WDF601                              
102608             END-IF                                                       
102708             PERFORM IMS-GHN-WDF601                                       
102808          END-PERFORM                                                     
102809        ELSE                                                              
102810**        DEFAULT LINE FOR CUSTOMER (CUSTOMER 999999)                     
102811**        IS UPDATED ON SCREEN.                                           
102812**        MEANS UPDATE ALL ORDERS WITH THIS DISTRICT, ORDERCLASS          
102813**        AND SUPPLIER WILL BE UPDATED ON WDF6 (ORDERS NOT PRINTED        
102814**        YET) EXCEPT FROM THE ORDERS THAT HAVE SPECIAL LINE FOR          
102815**        THAT CUSTOMER ON THIS SCREEN. (4431 MEANS WDF1)                 
102816**                                                                        
102817          MOVE LEVA-LEV-IDLEVNR   TO W-IDLEVNR-F6-MIN                     
102818                                     W-IDLEVNR-F6-MAX                     
102819          MOVE LEVA-DSTY-IDDISTR  TO W-IDDISTR-F6-MIN                     
102820                                     W-IDDISTR-F6-MAX                     
102821          MOVE LEVA-DSTY-KDORDKL  TO W-KDORDKL-F6-MIN                     
102822                                     W-KDORDKL-F6-MAX                     
102825                                                                          
102838                                                                          
102839          MOVE LEVA-DSTY-KVDAGAR-DIFF     TO WS-DSTY-KVDAGAR-DIFF         
102840          MOVE LEVA-DSTY-KVDAGAR-TPO                                      
102841                                    TO WS-DSTY-KVDAGAR-TPO                
102842                                                                          
102843          PERFORM IMS-GN-WDF6-MIN-MAX                                     
102845          PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                      
102847             IF PUDH-TITPO > ZERO                                         
102848             OR PUDH-TIREPDAT > ZERO                                      
102850                MOVE PUDH-IDLEVNR  TO W-IDDIRLEV-X                        
102851                MOVE PUDH-IDDISTR  TO W-IDDISTR                           
102852                MOVE PUDH-IDKUNDNR TO W-IDKUNDNR                          
102853                MOVE PUDH-KDORDKL  TO W-KDORDKL                           
102859                PERFORM IMS-GU-LEVA-DSTY                                  
102861                IF SEGMENT-SAKNAS                                         
102862                   PERFORM HAA-UPPDATERA-WDF601                           
102863                END-IF                                                    
102864             END-IF                                                       
102865             PERFORM IMS-GN-WDF6-MIN-MAX                                  
102866          END-PERFORM                                                     
102870        END-IF                                                            
102908     END-IF                                                               
103008     .                                                                    
103108     EJECT                                                                
103208 HAA-UPPDATERA-WDF601   SECTION.                                          
103308     MOVE 'HAA-UPD-WDF601  ' TO CURRENT-SECTION                           
103408                                                                          
103508     IF PUDH-TITPO > ZERO                                                 
103608        MOVE PUDH-TITPO          TO WORK-TIAAMMDD-TOM                     
103708     ELSE                                                                 
103808        MOVE PUDH-TIREPDAT       TO WORK-TIAAMMDD-TOM                     
103908     END-IF                                                               
104008                                                                          
104108     MOVE '11'                   TO WORK-IDDC                             
104208     MOVE 003                    TO WORK-KDCALL                           
104308     MOVE WS-DSTY-KVDAGAR-DIFF   TO WORK-KVWORKD                          
104408     ADD 1                       TO WORK-KVWORKD                          
104508     CALL WORKDAY USING WORK-KDCALL                                       
104608                        WORK-DATE-AREA                                    
104708                        WORK-KDSVAR                                       
104808                                                                          
104908     IF WORK-KDSVAR-FEL                                                   
105008        MOVE ' FEL FRÅN WORKDAY (W4043100) 1'                             
105108                                 TO ERROR-TEXT                            
105208        CALL ABEND USING RKOD-ABEND                                       
105308     ELSE                                                                 
105408        MOVE WORK-TIAAMMDD-FOM     TO WORK-TIAAMMDD-TOM                   
105508                                      WS-TISKEPPN                         
105608        MOVE WS-DSTY-KVDAGAR-TPO   TO WORK-KVWORKD                        
105708        ADD 1                      TO WORK-KVWORKD                        
105808        CALL WORKDAY USING WORK-KDCALL                                    
105908                           WORK-DATE-AREA                                 
106008                           WORK-KDSVAR                                    
106108                                                                          
106208        IF WORK-KDSVAR-FEL                                                
106308           MOVE ' FEL FRÅN WORKDAY (W4043100) 2'                          
106408                                     TO ERROR-TEXT                        
106508           CALL ABEND USING RKOD-ABEND                                    
106608        ELSE                                                              
106708           MOVE WORK-TIAAMMDD-FOM  TO WS-TISNDDAT                         
106808           PERFORM HAAA-KOLLA-JUSTERA-DATUM                               
106908           IF WS-DASKEPPN NOT = PUDH-DASKEPPN                             
107008              PERFORM HAAB-UPPDATERA-WDQ211                               
107108           END-IF                                                         
107208           MOVE WS-DASNDDAT        TO PUDH-DASNDDAT                       
107308           MOVE WS-DASKEPPN        TO PUDH-DASKEPPN                       
107408           PERFORM IMS-REPL-WDF601                                        
107508        END-IF                                                            
107608     END-IF                                                               
107708     .                                                                    
107808     EJECT                                                                
107908 HAAA-KOLLA-JUSTERA-DATUM  SECTION.                                       
108008     MOVE 'HAAA-KOLLA-DATUM' TO CURRENT-SECTION                           
108108                                                                          
108208*    DET KAN HANDA ATT VI FÅR ETT SÄNDDATUM SOM REDAN ÄR PASSERAT         
108308*    SÅ KAN VI NATURLIGTVIS INTE HA DET                                   
108408*    I DET FALLET SÄTTER VI SÄNDDATUM TILL DAGENS DATUM                   
108508*    OCH RÄKNAR OM SKEPPNINGSDATUM MED UTGÅNGSPUNKT FRÅN DET              
108608                                                                          
108708     IF WS-CURRENT-DATE (3:6) > WS-TISKEPPN                               
108808        MOVE WS-CURRENT-DATE       TO WS-DASNDDAT                         
108908        MOVE WS-CURRENT-DATE (3:6) TO WS-TISKEPPN                         
109008     ELSE                                                                 
109108        IF WS-CURRENT-DATE > WS-DASNDDAT                                  
109208           MOVE WS-CURRENT-DATE    TO WS-DASNDDAT                         
109308        END-IF                                                            
109408     END-IF                                                               
109508     .                                                                    
109608     EJECT                                                                
109708 HAAB-UPPDATERA-WDQ211  SECTION.                                          
109808     MOVE 'HAAB-UPD-WDQ211 ' TO CURRENT-SECTION                           
109908                                                                          
110008     MOVE PUDH-IDORDER       TO  W-IDORDER                                
110108     MOVE PUDH-IDDC          TO  W-IDDC                                   
110208     MOVE PUDH-IDLEVNR       TO  W-IDLEVNR                                
110308     PERFORM IMS-GHU-WDQ211                                               
110408     MOVE WS-TISKEPPN        TO  DIRL-TISKEPPN-DDC                        
110508     PERFORM IMS-REPL-WDQ211                                              
110608     .                                                                    
110708     EJECT                                                                
110808 S01-FORMATERING-MED-RDECDATA SECTION.                                    
110908     MOVE +2              TO DEC-KVHELTAL                                 
111008     MOVE +2              TO DEC-KVDECIMAL                                
111108     CALL WDECEDIT USING DEC-WDECAREA                                     
111208     .                                                                    
111308     EJECT                                                                
111408 MFS-RENSA-FAELT-UT SECTION.                                              
111508                                                                          
111608*    --- ALLA UTDATA-FÄLT                                                 
111708*    --- INKL. BLÄDDRINGSNYCKLAR                                          
111808     MOVE MFS-RENSA-FAELT TO MOD-CMD                                      
111908                             MOD-KDORDKL-E                                
112008                             MOD-IDDISTR-E                                
112108                             MOD-IDKUNDNR-E                               
112208                             MOD-IDDC-E                                   
112308                             MOD-KDVIA-E                                  
112408                             MOD-TIMINUT-CUT-E                            
112508                             MOD-KVDAGAR-LEV-E                            
112608                             MOD-KVDAGAR-TPO-E                            
112708                             MOD-KVDAGAR-DIFF-E                           
112808                             MOD-TECKEN                                   
112908     MOVE +1 TO INDX                                                      
113008     PERFORM UNTIL INDX > MAX-INDX                                        
113108       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
113208       ADD +1 TO INDX                                                     
113308     END-PERFORM                                                          
113408     .                                                                    
113508     SKIP3                                                                
113608 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
113708                                                                          
113808*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
113908                                                                          
114008     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL      (INDX)                      
114108                             MOD-IDDISTR      (INDX)                      
114208                             MOD-IDKUNDNR     (INDX)                      
114308                             MOD-IDDC         (INDX)                      
114408                             MOD-KDVIA        (INDX)                      
114508                             MOD-TIMINUT      (INDX)                      
114608                             MOD-KVDAGAR-LEV  (INDX)                      
114708                             MOD-KVDAGAR-TPO  (INDX)                      
114808                             MOD-KVDAGAR-DIFF (INDX)                      
114908     .                                                                    
115008     SKIP3                                                                
115108 MFS-RENSA-FAELT-IN SECTION.                                              
115208                                                                          
115308*    --- ALLA INDATA-FÄLT                                                 
115408     MOVE MFS-RENSA-FAELT TO   MOD-IDDIRLEV-IN                            
115508                               MOD-IDDISTR-IN                             
115608                               MOD-CMD                                    
115708                               MOD-KDORDKL-E                              
115808                               MOD-IDDISTR-E                              
115908                               MOD-IDKUNDNR-E                             
116008                               MOD-IDDC-E                                 
116108                               MOD-KDVIA-E                                
116208                               MOD-TIMINUT-CUT-E                          
116308                               MOD-KVDAGAR-LEV-E                          
116408                               MOD-KVDAGAR-TPO-E                          
116508                               MOD-KVDAGAR-DIFF-E                         
116608                               MOD-TECKEN                                 
116708                                                                          
116808     MOVE SPACE             TO MOD-KDORDKL-IN                             
116908     .                                                                    
117008     EJECT                                                                
117108 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
117208                                                                          
117308*    --- ALLA UTDATA-FÄLT                                                 
117408*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
117508     MOVE MFS-ROER-EJ-FAELT TO   MOD-KDORDKL-E                            
117608                                 MOD-IDDISTR-E                            
117708                                 MOD-IDKUNDNR-E                           
117808                                 MOD-IDDC-E                               
117908                                 MOD-KDVIA-E                              
118008                                 MOD-TIMINUT-CUT-E                        
118108                                 MOD-KVDAGAR-LEV-E                        
118208                                 MOD-KVDAGAR-TPO-E                        
118308                                 MOD-KVDAGAR-DIFF-E                       
118408                                 MOD-TECKEN                               
118508                                                                          
118608     MOVE +1 TO INDX                                                      
118708     PERFORM UNTIL INDX > MAX-INDX                                        
118808       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
118908       ADD +1 TO INDX                                                     
119008     END-PERFORM                                                          
119108     .                                                                    
119208     SKIP2                                                                
119308 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
119408                                                                          
119508*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
119608     MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDKL      (INDX)                    
119708                               MOD-IDDISTR      (INDX)                    
119808                               MOD-IDKUNDNR     (INDX)                    
119908                               MOD-IDDC         (INDX)                    
120008                               MOD-KDVIA        (INDX)                    
120108                               MOD-TIMINUT      (INDX)                    
120208                               MOD-KVDAGAR-LEV  (INDX)                    
120308                               MOD-KVDAGAR-TPO  (INDX)                    
120408                               MOD-KVDAGAR-DIFF (INDX)                    
120508                                                                          
120608     .                                                                    
120708     SKIP3                                                                
120808 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
120908                                                                          
121008*    --- ALLA INDATA-FÄLT                                                 
121108     MOVE MFS-ROER-EJ-FAELT TO   MOD-IDDIRLEV-IN                          
121208                                 MOD-IDDISTR-IN                           
121308                                 MOD-CMD                                  
121408                                 MOD-KDORDKL-E                            
121508                                 MOD-IDDISTR-E                            
121608                                 MOD-IDKUNDNR-E                           
121708                                 MOD-IDDC-E                               
121808                                 MOD-KDVIA-E                              
121908                                 MOD-TIMINUT-CUT-E                        
122008                                 MOD-KVDAGAR-LEV-E                        
122108                                 MOD-KVDAGAR-TPO-E                        
122208                                 MOD-KVDAGAR-DIFF-E                       
122308                                 MOD-TECKEN                               
122408     MOVE SPACE               TO MOD-KDORDKL-IN                           
122508     .                                                                    
122608     EJECT                                                                
122708 MFS-FORM-ATTR SECTION.                                                   
122808                                                                          
122908*    --- ALLA INDATA-FÄLT                                                 
123008     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR                              
123108                                MOD-KDORDKL-E-ATTR                        
123208                                MOD-IDDISTR-E-ATTR                        
123308                                MOD-IDKUNDNR-E-ATTR                       
123408                                MOD-IDDC-E-ATTR                           
123508                                MOD-KDVIA-E-ATTR                          
123608                                MOD-TIMINUT-CUT-E-ATTR                    
123708                                MOD-KVDAGAR-LEV-E-ATTR                    
123808                                MOD-KVDAGAR-TPO-E-ATTR                    
123908                                MOD-KVDAGAR-DIFF-E-ATTR                   
124008                                MOD-TECKEN-ATTR                           
124108     .                                                                    
124208     SKIP2                                                                
124308 MFS-LAES-IN-IGEN SECTION.                                                
124408                                                                          
124508*    --- ALLA INDATA-FÄLT                                                 
124608     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
124708                                   MOD-KDORDKL-E-ATTR                     
124808                                   MOD-IDDISTR-E-ATTR                     
124908                                   MOD-IDKUNDNR-E-ATTR                    
125008                                   MOD-IDDC-E-ATTR                        
125108                                   MOD-KDVIA-E-ATTR                       
125208                                   MOD-TIMINUT-CUT-E-ATTR                 
125308                                   MOD-KVDAGAR-LEV-E-ATTR                 
125408                                   MOD-KVDAGAR-TPO-E-ATTR                 
125508                                   MOD-KVDAGAR-DIFF-E-ATTR                
125608                                   MOD-TECKEN-ATTR                        
125708     .                                                                    
125808     EJECT                                                                
125908* --- IMS SEKTIONER ---                                                   
126008     SKIP3                                                                
126108 IMS-GET-MSG SECTION.                                                     
126208                                                                          
126308     MOVE '  QC' TO GODK-STATUSKODER                                      
126408     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126508     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126608     PERFORM IMS-STATUSKONTROLL                                           
126708     .                                                                    
126808     SKIP3                                                                
126908 IMS-INSERT-MSG SECTION.                                                  
127008                                                                          
127108*    IF MSGI-IDLAND-SPR = 'SE'                                            
127208*      MOVE '0' TO MFS-KDHUVOMR                                           
127308*    END-IF                                                               
127408     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
127508     MOVE SPACE TO GODK-STATUSKODER                                       
127608     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127708     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127808     PERFORM IMS-STATUSKONTROLL                                           
127908     .                                                                    
128008     EJECT                                                                
128108 IMS-GET-DUMMY-DISTR SECTION.                                             
128208     MOVE 'IMS-GET-DUMMY-DISTR  ' TO CURRENT-IMS-SECTION                  
128308                                                                          
128408     MOVE SPACE TO ALL-SSA                                                
128508     STRING 'WLLEVA01(IDLEVNR  =' W-IDDIRLEV-X ')'                        
128608          DELIMITED BY SIZE INTO SSA1                                     
128708     STRING 'WLLEVA18(WDF118KY =' W-WDF118KY-DUMMY-D ')'                  
128808          DELIMITED BY SIZE INTO SSA2                                     
128908     MOVE '  GE' TO GODK-STATUSKODER                                      
129008     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA18 SSA1 SSA2            
129108     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
129208     PERFORM IMS-STATUSKONTROLL                                           
129308     .                                                                    
129408     EJECT                                                                
129508 IMS-GET-DUMMY-CUST  SECTION.                                             
129608     MOVE 'IMS-GET-DUMMY-CUST   ' TO CURRENT-IMS-SECTION                  
129708                                                                          
129808     MOVE SPACE TO ALL-SSA                                                
129908     STRING 'WLLEVA01(IDLEVNR  =' W-IDDIRLEV-X ')'                        
130008          DELIMITED BY SIZE INTO SSA1                                     
130108     STRING 'WLLEVA18(WDF118KY =' W-WDF118KY-DUMMY-C ')'                  
130208          DELIMITED BY SIZE INTO SSA2                                     
130308     MOVE '  GE' TO GODK-STATUSKODER                                      
130408     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA18 SSA1 SSA2            
130508     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
130608     PERFORM IMS-STATUSKONTROLL                                           
130708     .                                                                    
130808     EJECT                                                                
130908 IMS-GET-LEVA SECTION.                                                    
131008     MOVE 'IMS-GET-LEVA    ' TO CURRENT-IMS-SECTION                       
131108                                                                          
131208     MOVE SPACE TO ALL-SSA                                                
131308     STRING 'WLLEVA01(IDLEVNR  =' W-IDDIRLEV-X ')'                        
131408          DELIMITED BY SIZE INTO SSA1                                     
131508     MOVE '  GE' TO GODK-STATUSKODER                                      
131608     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA01 SSA1                 
131708     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
131808     PERFORM IMS-STATUSKONTROLL                                           
131908     .                                                                    
132008     EJECT                                                                
132108 IMS-GET-LEVA-DSTY SECTION.                                               
132208     MOVE 'IMS-GET-LEVADSTY' TO CURRENT-IMS-SECTION                       
132308                                                                          
132408     MOVE SPACE TO ALL-SSA                                                
132508     STRING 'WLLEVA18(WDF118KY>=' W-WDF118KY-MIN-X                        
132608                    '&WDF118KY<=' W-WDF118KY-MAX-X ')'                    
132708          DELIMITED BY SIZE INTO SSA1                                     
132808     MOVE '  GE' TO GODK-STATUSKODER                                      
132908     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-WLLEVA18 SSA1                 
133008     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
133108     PERFORM IMS-STATUSKONTROLL                                           
133208     .                                                                    
133308     SKIP3                                                                
133309 IMS-GNP-LEVA-DSTY SECTION.                                               
133310     MOVE 'IMS-GNP-LEVA-DST' TO CURRENT-IMS-SECTION                       
133320                                                                          
133330     MOVE SPACE TO ALL-SSA                                                
133340     STRING 'WLLEVA18(WDF118KY>=' W-WDF118KY-MIN-X-L                      
133350                    '&WDF118KY<=' W-WDF118KY-MAX-X-H')'                   
133360          DELIMITED BY SIZE INTO SSA1                                     
133370     MOVE '  GE' TO GODK-STATUSKODER                                      
133380     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-WLLEVA18 SSA1                 
133390     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
133400     PERFORM IMS-STATUSKONTROLL                                           
133401     .                                                                    
133402     SKIP3                                                                
133408 IMS-GU-LEVA-DSTY SECTION.                                                
133508     MOVE 'IMS-GU-LEVA-DSTY' TO CURRENT-IMS-SECTION                       
133608                                                                          
133708     MOVE SPACE TO ALL-SSA                                                
133808     STRING 'WLLEVA01(IDLEVNR  =' W-IDDIRLEV-X ')'                        
133908          DELIMITED BY SIZE INTO SSA1                                     
134008     STRING 'WLLEVA18(WDF118KY =' W-WDF118KY-X ')'                        
134108                                                                          
134208          DELIMITED BY SIZE INTO SSA2                                     
134308     MOVE '  GE' TO GODK-STATUSKODER                                      
134408     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA18 SSA1 SSA2            
134508     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
134608     PERFORM IMS-STATUSKONTROLL                                           
134708     .                                                                    
134808     SKIP3                                                                
134908 IMS-ISRT-LEVA-DSTY SECTION.                                              
135008     MOVE 'IMS-ISRT-LEVADST' TO CURRENT-IMS-SECTION                       
135108                                                                          
135208     MOVE SPACE TO ALL-SSA                                                
135308*    STRING 'WLLEVA01(IDLEVNR  =' W-IDDIRLEV-X ')'                        
135408*         DELIMITED BY SIZE INTO SSA1                                     
135508     MOVE 'WLLEVA18 ' TO SSA1                                             
135608     MOVE '  II' TO GODK-STATUSKODER                                      
135708     CALL CBLTDLI USING ISRT LEVA-PCB DLI-IO-WLLEVA18 SSA1                
135808     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
135908     PERFORM IMS-STATUSKONTROLL                                           
136008     .                                                                    
136108     SKIP3                                                                
136208 IMS-REPL-LEVA-DSTY SECTION.                                              
136308     MOVE 'IMS-REPL-LEVADST' TO CURRENT-IMS-SECTION                       
136408                                                                          
136508     MOVE SPACE TO ALL-SSA                                                
136608     MOVE '  ' TO GODK-STATUSKODER                                        
136708     CALL CBLTDLI USING REPL LEVA-PCB DLI-IO-WLLEVA18                     
136808     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
136908     PERFORM IMS-STATUSKONTROLL                                           
137008     .                                                                    
137108     SKIP3                                                                
137208 IMS-DLET-LEVA-DSTY SECTION.                                              
137308     MOVE 'IMS-DLET-LEVADST' TO CURRENT-IMS-SECTION                       
137408                                                                          
137508     MOVE SPACE TO ALL-SSA                                                
137608     MOVE '  ' TO GODK-STATUSKODER                                        
137708     CALL CBLTDLI USING DLET LEVA-PCB DLI-IO-WLLEVA18                     
137808     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
137908     PERFORM IMS-STATUSKONTROLL                                           
138008     .                                                                    
138108     EJECT                                                                
138208 IMS-GHN-WDF601    SECTION.                                               
138308     MOVE 'IMS-GHN-WDF601  ' TO CURRENT-IMS-SECTION                       
138408                                                                          
138508     MOVE SPACE TO ALL-SSA                                                
138608     STRING 'WDF601  (WDF6ASEQ =' W-WDF6ASEQ-X ')'                        
138708          DELIMITED BY SIZE INTO SSA1                                     
138808     MOVE '  GEGB' TO GODK-STATUSKODER                                    
138908     CALL CBLTDLI USING GHN WDF6-PCB DLI-IO-WDF601 SSA1                   
139008     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
139108     PERFORM IMS-STATUSKONTROLL                                           
139208     .                                                                    
139308     EJECT                                                                
139408 IMS-REPL-WDF601    SECTION.                                              
139508     MOVE 'IMS-REPL-WDF601 ' TO CURRENT-IMS-SECTION                       
139608                                                                          
139708     MOVE SPACE TO ALL-SSA                                                
139808     MOVE '  ' TO GODK-STATUSKODER                                        
139908     CALL CBLTDLI USING REPL WDF6-PCB DLI-IO-WDF601                       
140008     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
140108     PERFORM IMS-STATUSKONTROLL                                           
140208     .                                                                    
140308     SKIP3                                                                
140408 IMS-GHU-WDQ211    SECTION.                                               
140508     MOVE 'IMS-GHU-WDQ211  ' TO CURRENT-IMS-SECTION                       
140608                                                                          
140708     MOVE SPACE TO ALL-SSA                                                
140808     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
140908          DELIMITED BY SIZE INTO SSA1                                     
141008     STRING 'WDQ211  (WDQ211KY =' W-WDQ211KY-X ')'                        
141108                                                                          
141208          DELIMITED BY SIZE INTO SSA2                                     
141308     MOVE '  ' TO GODK-STATUSKODER                                        
141408     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-WDQ211 SSA1 SSA2              
141508     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
141608     PERFORM IMS-STATUSKONTROLL                                           
141708     .                                                                    
141808     SKIP3                                                                
141908 IMS-REPL-WDQ211    SECTION.                                              
142008     MOVE 'IMS-REPL-WDQ211 ' TO CURRENT-IMS-SECTION                       
142108                                                                          
142208     MOVE SPACE TO ALL-SSA                                                
142308     MOVE '  ' TO GODK-STATUSKODER                                        
142408     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ211                       
142508     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
142608     PERFORM IMS-STATUSKONTROLL                                           
142708     .                                                                    
142808     SKIP3                                                                
142809 IMS-GN-WDF6-MIN-MAX SECTION.                                             
142810                                                                          
142820     STRING 'WDF601  (WDF6ASEQ>=' W-WDF6ASEQ-MIN-X                        
142830                    '&WDF6ASEQ<=' W-WDF6ASEQ-MAX-X ')'                    
142840          DELIMITED BY SIZE INTO SSA1                                     
142850     MOVE '  GEGB'             TO GODK-STATUSKODER                        
142860     CALL CBLTDLI USING GHN  WDF6-PCB DLI-IO-WDF601 SSA1                  
142870     MOVE WDF6-STATUS-CODE     TO STATUS-WS                               
142880     PERFORM IMS-STATUSKONTROLL                                           
142890     .                                                                    
142900     SKIP2                                                                
142901 IMS-GU-WDB201 SECTION.                                                   
142902                                                                          
142903     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
142904          DELIMITED BY SIZE INTO SSA1                                     
142905     MOVE '  GE' TO GODK-STATUSKODER                                      
142906     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
142907     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
142908     PERFORM IMS-STATUSKONTROLL                                           
142909     .                                                                    
142910     SKIP3                                                                
142920 IMS-STATUSKONTROLL SECTION.                                              
143008                                                                          
143108     SET STATUS-IX TO 1                                                   
143208     SEARCH GODK-STATUS                                                   
143308       AT END                                                             
143408         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
143508         DELIMITED BY SIZE INTO FELTEXT                                   
143608         CALL FELLOG                                                      
143708       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
143808         CONTINUE                                                         
144005     END-SEARCH                                                           
150000     .                                                                    
