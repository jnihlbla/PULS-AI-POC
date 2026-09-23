000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4061100.                                                
000400 AUTHOR.         SVANTE BJÖRKBERG.                                        
000500 DATE-WRITTEN.   JULI/AUGUSTI 1987.                                       
000600                                                                          
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        RELEASE PROGRAM FÖR FRAKTSEDEL DAGORDER SVERIGE                  
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T611 + W4T611U                                    
001400*        MID:         W4I61101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        TRANSAKTION: W4T694U                                             
001800*        MOD:         W4O61101 W4O69401                                   
001900     SKIP2                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77    PROGRAM-NAMN              PIC X(8).                                
002800 77    PGM-POS                   PIC X(08).                               
002900 77    JA                        PIC X       VALUE 'J'.                   
003000 77    NEJ                       PIC X       VALUE 'N'.                   
003100 77    SKRIVARE-NORDENKONTORET   PIC X       VALUE 'N'.                   
003200 77    SKRIVARE-SVERIGE2         PIC X       VALUE 'N'.                   
003300 77    FLKOMPL                   PIC X       VALUE 'N'.                   
003400 77    INSERT-ALT                PIC X       VALUE 'N'.                   
003500 77    LAST-EN-GANG              PIC X       VALUE 'N'.                   
003600 77    NYA-NYCKLAR               PIC X       VALUE 'J'.                   
003700 77    QUIT                      PIC X       VALUE 'Q'.                   
003800 77    PFK4                      PIC X       VALUE '4'.                   
003900 77    TRAFF                     PIC X       VALUE 'J'.                   
003910 77    SW-VISA-PA-BILD           PIC X       VALUE 'N'.                   
003920 77    WS-VORD-IDPRODNR-SAMP     PIC 9(7).                                
003930 77    WS-VORD-TIPACKN-SK        PIC 9(7).                                
004000 77    IDDISTR-WS                PIC X(4)    VALUE SPACE.                 
004100 77    IDKUNDNR-WS               PIC X(6)    VALUE SPACE.                 
004200 77    KDFRAKT-WS                PIC X(2)    VALUE SPACE.                 
004300 77    INDX                      PIC S9(2)   VALUE +0.                    
004400 77    IX                        PIC S9(2)   VALUE +0.                    
004500 77    RAD-LINE                  PIC S9(2)   VALUE +0.                    
004600 77    BILD-LINE                 PIC S9(2)   VALUE +0.                    
004700 77    JMF-IND                   PIC S9(2)   VALUE +0.                    
004800 77    PTOP-LINE                 PIC S9(2)   VALUE +0.                    
004900 77    MAX-LINE-PLUS-1           PIC S9(2)   VALUE +14.                   
005000 77    MAX-BILD-LINE             PIC S9(2)   VALUE +13.                   
005100 77    EGEN-MOD-LAENGD           PIC S9(4)   VALUE                        
005200                                                  +1013 COMP SYNC.        
005300 01  NYCKLAR-RAETT               PIC X.                                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500                                                                          
005600 01  WS-IDTRANS                  PIC X(4).                                
005700     88  EGEN-BILD                   VALUE '4611'.                        
005800     88  GODKAEND-BILD               VALUE '4611' '4614' '4694'.          
005900     88  UTSKRIFTS-BILD              VALUE '4694'.                        
006000     EJECT                                                                
006100 01  ARB-FAELT.                                                           
006200     03  WS-KVLADA-CONT          PIC S9(5).                               
006300     03  WS-KVPKT                PIC S9(5).                               
006400     03  WS-KVBNT-STY            PIC S9(5).                               
006500     03  WS-KVHACK               PIC S9(5).                               
006600     03  WS-KVPALL               PIC S9(5).                               
006700     03  WS-VKORDBTO             PIC S9(6)V9.                             
006800                                                                          
006900 01  WS-IDKUNDRF.                                                         
007000     03  WS-IDORDNR              PIC 9(5).                                
007100     03  FILLER                  PIC X(5).                                
007200     EJECT                                                                
007300                                                                          
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500     03 ABEND                    PIC X(8)  VALUE 'ABEND   '.              
007600     03 CBLTDLI                  PIC X(8)  VALUE 'CBLTDLI '.              
007700     03 FELLOG                   PIC X(8)  VALUE 'FELLOG  '.              
007710     03 W005INIT                 PIC X(8)  VALUE 'W005INIT'.              
007800     EJECT                                                                
007911*                                                                         
007912*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007913 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT '.           
007914*01 -COPY WMSGINIT                                                        
007915     SKIP2                                                                
007916*                                                                         
008000 01  ABENDKODER.                                                          
008100     03  FILLER                  PIC X(16) VALUE 'ABENDKODER'.            
008200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE   +16.         
008300     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
008400                                                                          
008500 01  KANTKODER-FOR-SAMPACKADE-ORDER.                                      
008600   03  TABELL-KANTKOD OCCURS 13 INDEXED BY TAB-LINE.                      
008700     05  TAB-IDPRODNR            PIC S9(7) COMP-3.                        
008800     05  TAB-KDUPPTYP            PIC  X(1).                               
008900     EJECT                                                                
009000                                                                          
009600 01    NYCKLAR-TILL-DLI.                                                  
009700                                                                          
009800   03    W-IDDISTR-MIN-X.                                                 
009900     05    W-IDDISTR-MIN         PIC S9(5)   VALUE ZERO  COMP-3.          
010000   03    W-IDDISTR-MAX-X.                                                 
010100     05    W-IDDISTR-MAX         PIC S9(5)   VALUE ZERO  COMP-3.          
010200                                                                          
010300   03    W-IDKUNDNR-MIN-X.                                                
010400     05    W-IDKUNDNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
010500   03    W-IDKUNDNR-MAX-X.                                                
010600     05    W-IDKUNDNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
010700                                                                          
010800   03    W-KDFRAKT-MIN-X.                                                 
010900     05    W-KDFRAKT-MIN         PIC S9(3)   VALUE ZERO  COMP-3.          
011000   03    W-KDFRAKT-MAX-X.                                                 
011100     05    W-KDFRAKT-MAX         PIC S9(3)   VALUE ZERO  COMP-3.          
011200                                                                          
011300   03    W-IDPRODNR-SAMP-MIN-X.                                           
011400     05    W-IDPRODNR-SAMP-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
011500   03    W-IDPRODNR-SAMP-MAX-X.                                           
011600     05    W-IDPRODNR-SAMP-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
011700                                                                          
011800   03  WD-WDE6D1KY-MIN-X.                                                 
011900     05  WD-IDDISTR-MIN          PIC S9(5)   VALUE ZERO  COMP-3.          
012000     05  WD-IDKUNDNR-MIN-X.                                               
012100       07  WD-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
012200     05  WD-KDFRAKT-MIN-X.                                                
012300       07  WD-KDFRAKT-MIN        PIC S9(3)   VALUE ZERO  COMP-3.          
012400     05  WD-IDPRODNR-SAMP-MIN-X.                                          
012500       07  WD-IDPRODNR-SAMP-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
012600     05  WD-IDPRODNR-MIN-X.                                               
012700       07  WD-IDPRODNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
012800                                                                          
012900   03  WD-WDE6D1KY-MAX-X.                                                 
013000     05  WD-IDDISTR-MAX          PIC S9(5)   VALUE ZERO  COMP-3.          
013100     05  WD-IDKUNDNR-MAX-X.                                               
013200       07  WD-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
013300     05  WD-KDFRAKT-MAX-X.                                                
013400       07  WD-KDFRAKT-MAX        PIC S9(3)   VALUE ZERO  COMP-3.          
013500     05  WD-IDPRODNR-SAMP-MAX-X.                                          
013600       07  WD-IDPRODNR-SAMP-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
013700     05  WD-IDPRODNR-MAX-X.                                               
013800       07  WD-IDPRODNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
013900                                                                          
014000   03    WE6-IDPRODNR-X.                                                  
014100     05    WE6-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
014200                                                                          
014300   03    W-IDDC-X.                                                        
014400     05    W-IDDC                PIC X(2).                                
014500                                                                          
014600   03    W-KDORDKL-MIN-X.                                                 
014700     05    W-KDORDKL-MIN         PIC S9      VALUE +0    COMP-3.          
014800   03    W-KDORDKL-MAX-X.                                                 
014900     05    W-KDORDKL-MAX         PIC S9      VALUE +3    COMP-3.          
014910                                                                          
014920   03  W-IDDC-B6-X.                                                       
014930       05 W-IDDC-B6                  PIC X(2).                            
015000                                                                          
015100     SKIP3                                                                
015200 01    MEDDELANDE.                                                        
015300   03    FEL-1                   PIC X(40)   VALUE                        
015400             ' FEL NYCKEL  '.                                             
015500   03    FEL-2                   PIC X(40)   VALUE                        
015600             ' EJ VAL OCH NYA NYCKLAR  '.                                 
015700   03    FEL-3                   PIC X(40)   VALUE                        
015800             ' PF11 FÖR UPPDATERING  '.                                   
015900   03    FEL-4                   PIC X(40)   VALUE                        
016000             ' UPPLYSTA FÄLT FEL   '.                                     
016100   03    FEL-5                   PIC X(40)   VALUE                        
016200             ' FYLL I VALFÄLT      '.                                     
016300   03    FEL-6                   PIC X(40)   VALUE                        
016400             ' ENDAST "J" ELLER "N" FÖR SIDVAL   '.                       
016500   03    FEL-7                   PIC X(40)   VALUE                        
016600             ' ENDAST KOMPLETERING FÖR EN ORDER '.                        
016700   03    FEL-8                   PIC X(40)   VALUE                        
016800             ' ENDAST "K" EL "V" EL "B" '.                                
016900   03    FEL-9                   PIC X(40)   VALUE                        
017000             ' ENDAST "E" EL "B" '.                                       
017100   03    FEL-10                  PIC X(40)   VALUE                        
017200             ' ORDER SAKNAS  '.                                           
017300   03    FEL-11                  PIC X(40)   VALUE                        
017400             ' SAMPACKAD ORDER'.                                          
017500   03    FEL-12                  PIC X(40)   VALUE                        
017600             ' FLER ORDER FINNS'.                                         
017700   03    FEL-13                  PIC X(40)   VALUE                        
017800             ' VALDA DISTRIKT TILLHÖR OLIKA AVDELN'.                      
017900   03    FEL-14                  PIC X(40)   VALUE                        
018000             ' SAMPACKAD ORDER SAKNAS'.                                   
018100   03    FEL-15                  PIC X(40)   VALUE                        
018200             'FRAKTKOD MÅSTE VARA SAMMA FÖR ALLA RADER'.                  
018230   03    FEL-16                  PIC X(40)   VALUE                        
018240             'SCREEN 4611 ONLY FOR CDC AND LDC-SE.'.                      
018250   03    FEL-17                  PIC X(40)   VALUE                        
018260             'DC (LAGERTILLHÖRIGHET) SAKNAS.    '.                        
018300     EJECT                                                                
018400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
018500*                                                                         
018600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
018700                                                                          
018800*01    -COPY WMSGAREA                                                     
018900     EJECT                                                                
019000*  03    MOD -COPY W4O61101  -RED MSG-AREA  -PRE MOD-                     
019100     EJECT                                                                
019200 01    FILLER                    PIC X(16)   VALUE 'MID-AREA'.            
019300                                                                          
019400*01    MID -COPY W4I61101                                                 
019500    SKIP2                                                                 
019600 01    FILLER                    PIC X(16)   VALUE 'WWDIST-AREA'.         
019700 01  TEST-IDDISTR                PIC 9(5)                COMP-3.          
019800*01  FILLER -COPY WWDIST80       -RED TEST-IDDISTR.                       
019900    SKIP2                                                                 
020000*01    -COPY WWFRAKT1                                                     
020100    SKIP2                                                                 
020200                                                                          
020300 01  FILLER                      PIC X(16) VALUE 'P-TO-P-SW'.             
020400 01  P-TO-P-SW.                                                           
020500   03  PTOP-LL                   PIC S9(4) VALUE +400 COMP SYNC.          
020600   03  PTOP-Z1                   PIC X     VALUE LOW-VALUE.               
020700   03  PTOP-Z2                   PIC X     VALUE LOW-VALUE.               
020800   03  PTOP-TRANSKOD             PIC X(7)  VALUE 'W4T694U'.               
020900   03  FILLER                    PIC X     VALUE SPACE.                   
021000   03  FILLER                    PIC X(4)  VALUE '4611'.                  
021100   03  PTOP-KDMFSFOR             PIC X     VALUE '1'.                     
021200*  03    MOD -COPY W4I69401  -PRE PTOP-                                   
021300     EJECT                                                                
021400                                                                          
021500*01    -COPY WMFSAREA                                                     
021600     EJECT                                                                
021700*                                                                         
021800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000 01    IMS-WS.                                                            
022100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
022200     SKIP3                                                                
022300*                        **** STATUS-KOD FRÅN IMS                         
022400   03    STATUS-WS               PIC XX.                                  
022500     88    SEGMENT-FINNS                     VALUE '  '.                  
022600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
022700     88    BASEN-SLUT                        VALUE 'GB'.                  
022800     SKIP3                                                                
022900   03    GODK-STATUSKODER.                                                
023000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
023100     SKIP3                                                                
023200 01    SSA1                      PIC X(128).                              
023300 01    SSA2                      PIC X(64).                               
023400     EJECT                                                                
023500*                            IMS FUNKTIONSKODER                           
023600*01    -COPY W0003                                                        
023700     EJECT                                                                
023800*                            DLI INPUT-OUTPUT AREA                        
023810 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E601'.         
023820 01    DLI-IO-E601.                                                       
023830*  03    -COPY WDE601                                                     
023840     EJECT                                                                
023850 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E611'.         
023860 01    DLI-IO-E611.                                                       
023870*  03    -COPY WDE611                                                     
023880     EJECT                                                                
023890 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E6D1'.         
023891 01    DLI-IO-E6D1.                                                       
023892*  03    -COPY WDE6D1                                                     
023893     EJECT                                                                
023894 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E401'.         
023895 01    DLI-IO-E401.                                                       
023896*  03    -COPY WDE401                                                     
023897     EJECT                                                                
023898 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023899 01   DLI-IO-AREA-B601.                                                   
023900*     03  -COPY WDB601                                                    
024000     EJECT                                                                
025000 LINKAGE SECTION.                                                         
025100*01    -COPY W0009     -PRE MSG-                                          
025200     EJECT                                                                
025300*01    -COPY W0009     -PRE ALT-                                          
025400     EJECT                                                                
025500*01    -COPY W0008     -PRE WDP7-                                         
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025710*01    -COPY W0008     -PRE WDE6-                                         
025720     05  FILLER                  PIC X.                                   
025730     EJECT                                                                
025740*01    -COPY W0008     -PRE WDE4-                                         
025750     05  FILLER                  PIC X.                                   
025760     EJECT                                                                
025800*01    -COPY W0008     -PRE WDE6D-                                        
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026010*01    -COPY W0008     -PRE WDB6-                                         
026020     05  FILLER                  PIC X.                                   
026030     EJECT                                                                
026100 PROCEDURE DIVISION USING MSG-PCB   ALT-PCB                               
026200                          WDP7-PCB  WDE6-PCB  WDE4-PCB                    
026210                          WDE6D-PCB WDB6-PCB.                             
026300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
026400                           WDP7-PCB  WDE6-PCB  WDE4-PCB                   
026410                           WDE6D-PCB WDB6-PCB.                            
026500     PERFORM IMS-GET-MSG                                                  
026600     IF SEGMENT-FINNS                                                     
026700       PERFORM A-INIT                                                     
026800       IF NYCKLAR-OK                                                      
026900         IF EGEN-BILD                                                     
027000           IF NYA-NYCKLAR = JA                                            
027100             IF MFS-IDPFK = PFK4                                          
027200               MOVE FEL-2 TO MOD-TEMFSFEL                                 
027300             ELSE                                                         
027400               PERFORM B-LAS-ORDER                                        
027500             END-IF                                                       
027600           ELSE                                                           
027700             IF MFS-IDPFK = PFK4                                          
027710*BÅDE PF4 OCH PF11 GER UTSKRIFT AV FRAKTSEDEL PÅ 4611.                    
027800               PERFORM D-STARTA-RELEASE                                   
027900             ELSE                                                         
028000               IF MFS-IDPFK = '7' OR '8'                                  
028100                 PERFORM S05-KOLL-UPPDAT-FAELT                            
028200                 IF NYCKLAR-OK                                            
028300                   IF MFS-IDPFK = '8'                                     
028400                     PERFORM C-LAS-VIDARE                                 
028500                   END-IF                                                 
028600                   PERFORM B-LAS-ORDER                                    
028700                 ELSE                                                     
028800                   PERFORM S04-SAMMA-BILD                                 
028900                   MOVE FEL-4 TO MOD-TEMFSFEL                             
029000                 END-IF                                                   
029100               ELSE                                                       
029200                 IF MFS-IDPFK = SPACE                                     
029300                   PERFORM S05-KOLL-UPPDAT-FAELT                          
029400                   PERFORM G-KONTROLL-HEL-SIDA                            
029500                   IF NYCKLAR-OK                                          
029600                     PERFORM F-ENTER-GAMLA-NYCKLAR                        
029700                     PERFORM B-LAS-ORDER                                  
029800                   ELSE                                                   
029900                     PERFORM S04-SAMMA-BILD                               
030000                   END-IF                                                 
030100                 END-IF                                                   
030200               END-IF                                                     
030300             END-IF                                                       
030400           END-IF                                                         
030500         ELSE                                                             
030600           IF UTSKRIFTS-BILD                                              
030700             PERFORM B-LAS-ORDER                                          
030800           ELSE                                                           
030900             PERFORM E-RENSA-NYCKLAR                                      
031000           END-IF                                                         
031100         END-IF                                                           
031200       ELSE                                                               
031300         IF NOT EGEN-BILD                                                 
031400           PERFORM E-RENSA-NYCKLAR                                        
031500         END-IF                                                           
031600       END-IF                                                             
031700       IF INSERT-ALT = JA                                                 
031800         PERFORM IMS-INSERT-ALT                                           
031900       ELSE                                                               
032000         MOVE 'W4O61101'      TO MFS-IDMOD                                
032100         MOVE '4611'          TO MFS-IDTRANS                              
032200         MOVE EGEN-MOD-LAENGD TO MSG-KVLL                                 
032300         PERFORM IMS-INSERT-MSG                                           
032400       END-IF                                                             
032500     END-IF                                                               
032600                                                                          
032700     MOVE ZERO TO RETURN-CODE                                             
032800     GOBACK                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 A-INIT             SECTION.                                              
033200                                                                          
033300     IF MSG-DUBBLA-TRANSKODER                                             
033400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I61101-CTX             
033500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
033600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
033800     ELSE                                                                 
033900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I61101-CTX              
034000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
034100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034200       MOVE ' ' TO MFS-KDTRTYP                                            
034300     END-IF                                                               
034400     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
034500                                                                          
034600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
034700     MOVE LOW-VALUE TO MSG-AREA                                           
034800                                                                          
034900     MOVE '4611'    TO MOD-IDTRANS                                        
035000                                                                          
035100     MOVE +0          TO W-KDORDKL-MIN                                    
035200     MOVE +3          TO W-KDORDKL-MAX                                    
035300     MOVE +13         TO MAX-BILD-LINE                                    
035400     MOVE NEJ         TO SKRIVARE-SVERIGE2                                
035500                         SKRIVARE-NORDENKONTORET                          
035513                         SW-VISA-PA-BILD                                  
035520     MOVE JA          TO NYCKLAR-RAETT                                    
035600                                                                          
035700     IF SWEDISH-TEXT                                                      
035800       MOVE +1 TO INDX                                                    
035900     ELSE                                                                 
036000       MOVE +2 TO INDX                                                    
036100     END-IF                                                               
036200                                                                          
036300     MOVE 'W4061100'   TO PROGRAM-NAMN                                    
036400                                                                          
036500     MOVE ALL '+'               TO MSGI-WMSGINIT                          
036600     MOVE '001'                 TO MSGI-KDCALL                            
036700     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
036800     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
036900     MOVE '4611'                TO MSGI-IDTRANS                           
037000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
037200                                                                          
039100     IF MID-IDDISTR-IN   = ALL '+'  AND                                   
039200        MID-IDKUNDNR-IN  = ALL '+'  AND                                   
039300        MID-KDFRAKT-IN   = ALL '+'                                        
039400       MOVE NEJ TO NYA-NYCKLAR                                            
039500     END-IF                                                               
039600                                                                          
039610     MOVE MSGI-IDDC           TO W-IDDC                                   
039620                                                                          
039700     PERFORM AA-SPARA-INPUT                                               
039800     PERFORM AC-RENSA-FAELT                                               
039900     PERFORM AB-KOLLA-INPUT                                               
040000     .                                                                    
040100     EJECT                                                                
040200                                                                          
040300 AA-SPARA-INPUT  SECTION.                                                 
040400                                                                          
040500     IF MID-IDDISTR-IN = ALL '+'                                          
040600       IF NYA-NYCKLAR = JA                                                
040700         MOVE ZERO            TO IDDISTR-WS                               
040800       ELSE                                                               
040900         MOVE MID-IDDISTR-UT  TO IDDISTR-WS                               
041000       END-IF                                                             
041100                                                                          
041200       INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                     
041300     ELSE                                                                 
041400       MOVE MID-IDDISTR-IN TO IDDISTR-WS                                  
041500     END-IF                                                               
041600                                                                          
041700     MOVE IDDISTR-WS TO MOD-IDDISTR-UT                                    
041800     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
041900                                                                          
042000     IF MID-IDKUNDNR-IN = ALL '+'                                         
042100       IF NYA-NYCKLAR = JA                                                
042200         MOVE ZERO            TO IDKUNDNR-WS                              
042300       ELSE                                                               
042400         MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                              
042500       END-IF                                                             
042600                                                                          
042700       INSPECT IDKUNDNR-WS REPLACING ALL SPACE BY ZERO                    
042800     ELSE                                                                 
042900       MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                                
043000     END-IF                                                               
043100                                                                          
043200     MOVE IDKUNDNR-WS TO MOD-IDKUNDNR-UT                                  
043300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
043400                                                                          
043500     IF MID-KDFRAKT-IN = ALL '+'                                          
043600       IF NYA-NYCKLAR = JA                                                
043700         MOVE ZERO            TO KDFRAKT-WS                               
043800       ELSE                                                               
043900         MOVE MID-KDFRAKT-UT  TO KDFRAKT-WS                               
044000       END-IF                                                             
044100                                                                          
044200       INSPECT KDFRAKT-WS REPLACING ALL SPACE BY ZERO                     
044300     ELSE                                                                 
044400       MOVE MID-KDFRAKT-IN TO KDFRAKT-WS                                  
044500     END-IF                                                               
044600                                                                          
044700     MOVE KDFRAKT-WS TO MOD-KDFRAKT-UT                                    
044800     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
044900     SKIP3                                                                
045000                                                                          
045100     MOVE 1 TO RAD-LINE                                                   
045200                                                                          
045300     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
045400       INSPECT MID-IDDISTR  (RAD-LINE)                                    
045500               REPLACING LEADING SPACE BY ZERO                            
045600       INSPECT MID-IDKUNDNR (RAD-LINE)                                    
045700               REPLACING LEADING SPACE BY ZERO                            
045800       INSPECT MID-IDORDNR  (RAD-LINE)                                    
045900               REPLACING LEADING SPACE BY ZERO                            
046000       INSPECT MID-KDORDKL  (RAD-LINE)                                    
046100               REPLACING LEADING SPACE BY ZERO                            
046200       INSPECT MID-IDPRODNR (RAD-LINE)                                    
046300               REPLACING LEADING SPACE BY ZERO                            
046400       ADD +1 TO RAD-LINE                                                 
046500     END-PERFORM                                                          
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900 AB-KOLLA-INPUT   SECTION.                                                
047000                                                                          
047010     IF MSGI-IDDC IS > SPACE                                              
047020       MOVE MSGI-IDDC           TO MOD-IDDC-UT                            
047021                                   W-IDDC-B6                              
047022                                   W-IDDC                                 
047023       PERFORM IMS-GU-WDB601                                              
047030       IF DCS-CDC                                                         
047031       OR DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
047040         CONTINUE                                                         
047050       ELSE                                                               
047060         MOVE NEJ               TO NYCKLAR-RAETT                          
047070         MOVE FEL-16            TO MOD-TEMFSFEL                           
047080       END-IF                                                             
047090     ELSE                                                                 
047091       MOVE NEJ                 TO NYCKLAR-RAETT                          
047092       MOVE FEL-17              TO MOD-TEMFSFEL                           
047093     END-IF                                                               
047094                                                                          
047100     IF IDDISTR-WS  NUMERIC  AND                                          
047200        IDKUNDNR-WS NUMERIC  AND                                          
047300        KDFRAKT-WS  NUMERIC                                               
047400       IF IDDISTR-WS  > 0000   OR                                         
047500          IDKUNDNR-WS > 000000 OR                                         
047600          KDFRAKT-WS  > 00                                                
047610         CONTINUE                                                         
047800       ELSE                                                               
047810         IF NYCKLAR-OK                                                    
047900           MOVE NEJ TO NYCKLAR-RAETT                                      
048000           MOVE FEL-1 TO MOD-TEMFSFEL                                     
048100         END-IF                                                           
048110       END-IF                                                             
048200     ELSE                                                                 
048210       IF NYCKLAR-OK                                                      
048300         MOVE NEJ TO NYCKLAR-RAETT                                        
048400         MOVE FEL-1 TO MOD-TEMFSFEL                                       
048500       END-IF                                                             
048510     END-IF                                                               
048600                                                                          
048700     MOVE +1              TO IX                                           
048800                                                                          
048900     PERFORM UNTIL IX = MAX-LINE-PLUS-1                                   
049000       IF MID-KDUPPTYP (IX) = ' ' OR '+'                                  
049100         CONTINUE                                                         
049200       ELSE                                                               
049300         IF MID-RADTEXT (IX) = 'SAMP'                                     
049400           IF MID-KDUPPTYP (IX) = 'B'                                     
049500             CONTINUE                                                     
049600           ELSE                                                           
049610             CONTINUE                                                     
049700*            MOVE MFS-ALFA-FAELT-FEL TO                                   
049800*                 MOD-KDUPPTYP-ATTR (IX)                                  
049900*            MOVE FEL-11             TO MOD-TEMFSFEL                      
050000*            MOVE NEJ                TO NYCKLAR-RAETT                     
050100*            PERFORM S04-SAMMA-BILD                                       
050200           END-IF                                                         
050300         ELSE                                                             
050400           IF MID-SPAR-IDPRODNR-SAMP NUMERIC        AND                   
050500              MID-SPAR-IDPRODNR-SAMP = MID-IDPRODNR (IX)                  
050600             MOVE MFS-ALFA-FAELT-FEL TO                                   
050700                  MOD-KDUPPTYP-ATTR (IX)                                  
050800             MOVE FEL-12             TO MOD-TEMFSFEL                      
050900             MOVE NEJ                TO NYCKLAR-RAETT                     
051000             PERFORM S04-SAMMA-BILD                                       
051100           END-IF                                                         
051200         END-IF                                                           
051300       END-IF                                                             
051400                                                                          
051500       ADD +1              TO IX                                          
051600     END-PERFORM                                                          
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 AC-RENSA-FAELT   SECTION.                                                
052100                                                                          
052200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
052300                             MOD-IDKUNDNR-IN                              
052400                             MOD-KDFRAKT-IN                               
052500                             MOD-TEMFSFEL                                 
052600                             MOD-TEMFSINF                                 
052700                                                                          
052800     MOVE ZERO            TO WS-KVLADA-CONT                               
052900                             WS-KVPKT                                     
053000                             WS-KVBNT-STY                                 
053100                             WS-KVHACK                                    
053200                             WS-KVPALL                                    
053300                             WS-VKORDBTO                                  
053400     .                                                                    
053500     EJECT                                                                
053600                                                                          
053700 B-LAS-ORDER SECTION.                                                     
053800                                                                          
053900     MOVE LOW-VALUE          TO W-IDPRODNR-SAMP-MIN-X                     
054000                                WD-IDPRODNR-SAMP-MIN-X                    
054100                                WD-IDPRODNR-MIN-X                         
054200     MOVE HIGH-VALUE         TO W-IDPRODNR-SAMP-MAX-X                     
054300                                WD-IDPRODNR-SAMP-MAX-X                    
054400                                WD-IDPRODNR-MAX-X                         
054500                                                                          
054600                                                                          
054700     EVALUATE TRUE                                                        
054800       WHEN IDDISTR-WS > ZERO AND KDFRAKT-WS > ZERO                       
054900          PERFORM BA-LAS-DIST-FRAKT                                       
055000       WHEN IDDISTR-WS = ZERO AND KDFRAKT-WS > ZERO                       
055100          PERFORM BB-LAS-FRAKT                                            
055200       WHEN IDDISTR-WS > ZERO                                             
055300          PERFORM BC-LAS-DISTR                                            
055400       WHEN OTHER                                                         
055500         MOVE FEL-1 TO MOD-TEMFSFEL                                       
055600     END-EVALUATE                                                         
055700                                                                          
055800     IF SEGMENT-FINNS                                                     
055900        MOVE SEQD-IDDISTR       TO MOD-SPAR-IDDISTR                       
056000        MOVE SEQD-IDKUNDNR      TO MOD-SPAR-IDKUNDNR                      
056100        MOVE SEQD-KDFRAKT       TO MOD-SPAR-KDFRAKT                       
056200        MOVE SEQD-IDPRODNR-SAMP TO MOD-SPAR-IDPRODNR-SAMP                 
056300        MOVE 'FLER FINNS'       TO MOD-TEMFSINF                           
056400     ELSE                                                                 
056500        IF RAD-LINE = 1                                                   
056600           MOVE FEL-10           TO MOD-TEMFSFEL                          
056700        END-IF                                                            
056800                                                                          
056900        PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                          
057000           MOVE MFS-STAENG-FAELT TO MOD-KDUPPTYP-ATTR (RAD-LINE)          
057100           ADD +1                TO RAD-LINE                              
057200        END-PERFORM                                                       
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 BA-LAS-DIST-FRAKT SECTION.                                               
057800                                                                          
057900     IF IDKUNDNR-WS > 0                                                   
058000       MOVE IDDISTR-WS  TO WD-IDDISTR-MIN                                 
058100                           WD-IDDISTR-MAX                                 
058200       MOVE IDKUNDNR-WS TO WD-IDKUNDNR-MIN                                
058300                           WD-IDKUNDNR-MAX                                
058400       MOVE KDFRAKT-WS  TO WD-KDFRAKT-MIN                                 
058500                           WD-KDFRAKT-MAX                                 
058600                                                                          
058700       IF LAST-EN-GANG = NEJ                                              
058800         PERFORM IMS-GN-WDE6D-MED-KNDNR                                   
058900       END-IF                                                             
059000     ELSE                                                                 
059100       MOVE IDDISTR-WS  TO W-IDDISTR-MIN                                  
059200       MOVE LOW-VALUE   TO W-IDKUNDNR-MIN-X                               
059300       MOVE HIGH-VALUE  TO W-IDKUNDNR-MAX-X                               
059400       MOVE KDFRAKT-WS  TO W-KDFRAKT-MIN                                  
059500                                                                          
059600       IF LAST-EN-GANG = NEJ                                              
059700         PERFORM IMS-GN-WDE6D                                             
059800       END-IF                                                             
059900     END-IF                                                               
060000                                                                          
060100     MOVE +1          TO RAD-LINE                                         
060200                                                                          
060300     PERFORM UNTIL NOT SEGMENT-FINNS          OR                          
060400                   RAD-LINE = MAX-LINE-PLUS-1                             
060500                                                                          
060600       MOVE SEQD-IDPRODNR TO WE6-IDPRODNR                                 
060700       PERFORM IMS-GU-WDE601                                              
060800       PERFORM S01-FLYTTA-HUVUD-POST                                      
060900       PERFORM IMS-GNP-WDE611                                             
061000                                                                          
061100       PERFORM UNTIL NOT SEGMENT-FINNS                                    
061200         PERFORM S02-ADDERA-KOLLI                                         
061300         PERFORM IMS-GNP-WDE611                                           
061400       END-PERFORM                                                        
061500                                                                          
061600       PERFORM IMS-GU-WDE401-ESEQ                                         
061700       MOVE KORD-IDKUNDRF  TO WS-IDKUNDRF                                 
061800       MOVE WS-IDORDNR     TO MOD-IDORDNR(RAD-LINE)                       
061900                                                                          
062000       PERFORM S03-FLYTTA-TILL-MOD                                        
062100       ADD +1 TO RAD-LINE                                                 
062200                                                                          
062300       IF IDKUNDNR-WS > 0                                                 
062400         PERFORM IMS-GN-WDE6D-MED-KNDNR                                   
062500       ELSE                                                               
062600         PERFORM IMS-GN-WDE6D                                             
062700       END-IF                                                             
062800     END-PERFORM                                                          
062900     .                                                                    
063000     EJECT                                                                
063100 BB-LAS-FRAKT SECTION.                                                    
063200                                                                          
063300     MOVE KDFRAKT-WS      TO W-KDFRAKT-MIN                                
063400                                                                          
063500     IF LAST-EN-GANG = NEJ                                                
063600       PERFORM IMS-GN-WDE6D-FRAKT                                         
063700     END-IF                                                               
063800                                                                          
063900     MOVE +1              TO RAD-LINE                                     
064000                                                                          
064100     PERFORM UNTIL NOT SEGMENT-FINNS          OR                          
064200                   RAD-LINE = MAX-LINE-PLUS-1                             
064300       MOVE SEQD-IDPRODNR TO WE6-IDPRODNR                                 
064400       PERFORM IMS-GU-WDE601                                              
064500       PERFORM S01-FLYTTA-HUVUD-POST                                      
064600       PERFORM IMS-GNP-WDE611                                             
064700                                                                          
064800       PERFORM UNTIL NOT SEGMENT-FINNS                                    
064900         PERFORM S02-ADDERA-KOLLI                                         
065000         PERFORM IMS-GNP-WDE611                                           
065100       END-PERFORM                                                        
065200                                                                          
065300       PERFORM IMS-GU-WDE401-ESEQ                                         
065400       MOVE KORD-IDKUNDRF  TO WS-IDKUNDRF                                 
065500       MOVE WS-IDORDNR     TO MOD-IDORDNR(RAD-LINE)                       
065600                                                                          
065700       PERFORM S03-FLYTTA-TILL-MOD                                        
065800       ADD +1 TO RAD-LINE                                                 
065900       PERFORM IMS-GN-WDE6D-FRAKT                                         
066000     END-PERFORM                                                          
066100     .                                                                    
066200     EJECT                                                                
066300 BC-LAS-DISTR SECTION.                                                    
066400                                                                          
066500     MOVE LOW-VALUE       TO WD-WDE6D1KY-MIN-X                            
066600     MOVE HIGH-VALUE      TO WD-WDE6D1KY-MAX-X                            
066700                                                                          
066800     MOVE IDDISTR-WS      TO WD-IDDISTR-MIN                               
066900                             WD-IDDISTR-MAX                               
067000                                                                          
067100     IF IDKUNDNR-WS > ZERO                                                
067200       MOVE IDKUNDNR-WS     TO WD-IDKUNDNR-MIN                            
067300                               WD-IDKUNDNR-MAX                            
067400     END-IF                                                               
067500                                                                          
067600     IF LAST-EN-GANG = NEJ                                                
067700       PERFORM IMS-GN-WDE6D-DISTR                                         
067800     END-IF                                                               
067900                                                                          
068000     MOVE +1          TO RAD-LINE                                         
068100                                                                          
068200     PERFORM UNTIL NOT SEGMENT-FINNS OR                                   
068300                   RAD-LINE = MAX-LINE-PLUS-1                             
068400        MOVE SEQD-IDPRODNR TO WE6-IDPRODNR                                
068500        PERFORM IMS-GU-WDE601                                             
068600        PERFORM S01-FLYTTA-HUVUD-POST                                     
068700        PERFORM IMS-GNP-WDE611                                            
068800                                                                          
068900        PERFORM UNTIL NOT SEGMENT-FINNS                                   
069000           PERFORM S02-ADDERA-KOLLI                                       
069100           PERFORM IMS-GNP-WDE611                                         
069200        END-PERFORM                                                       
069300                                                                          
069400        PERFORM IMS-GU-WDE401-ESEQ                                        
069500        MOVE KORD-IDKUNDRF  TO WS-IDKUNDRF                                
069600        MOVE WS-IDORDNR     TO MOD-IDORDNR(RAD-LINE)                      
069700                                                                          
069800        PERFORM S03-FLYTTA-TILL-MOD                                       
069900        ADD +1 TO RAD-LINE                                                
070000                                                                          
070100        PERFORM IMS-GN-WDE6D-DISTR                                        
070200     END-PERFORM                                                          
070300     .                                                                    
070400     EJECT                                                                
070500 C-LAS-VIDARE SECTION.                                                    
070600                                                                          
070700     IF MID-SPAR-IDDISTR  NUMERIC AND                                     
070800        MID-SPAR-IDKUNDNR NUMERIC AND                                     
070900        MID-SPAR-KDFRAKT  NUMERIC AND                                     
071000        MID-SPAR-IDPRODNR-SAMP NUMERIC                                    
071100       MOVE MID-SPAR-IDDISTR       TO W-IDDISTR-MIN                       
071200       MOVE MID-SPAR-IDKUNDNR      TO W-IDKUNDNR-MIN                      
071300       MOVE MID-SPAR-KDFRAKT       TO W-KDFRAKT-MIN                       
071400       MOVE MID-SPAR-IDPRODNR-SAMP TO W-IDPRODNR-SAMP-MIN                 
071500       PERFORM IMS-GU-WDE6D                                               
071600       MOVE JA                TO LAST-EN-GANG                             
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 D-STARTA-RELEASE SECTION.                                                
072100*        KDUPPTYP 'B' = 'BORTTAG'                                         
072200*        KDUPPTYP 'E' = '???' EJ UTSKRIFT?? INGEN VERKAR VETA/ANVÄ        
072300*        KDUPPTYP 'K' = 'KORRIGERA'                                       
072400*        KDUPPTYP 'V' = 'VÄLJ'                                            
072500                                                                          
072600     MOVE ALL '+' TO PTOP-W4I69401-CTX                                    
072700     MOVE JA TO NYCKLAR-RAETT                                             
072800     PERFORM DD-SATT-KANTKOD-FOR-SAMP-ORDER                               
072900                                                                          
073000     IF NYCKLAR-OK                                                        
073100       EVALUATE TRUE                                                      
073200         WHEN MID-FLSAMTL = 'J'                                           
073300           PERFORM DA-ALLT-VALT                                           
073400         WHEN MID-FLSAMTL = 'N'                                           
073500           PERFORM DB-DELVIS-VAL                                          
073600         WHEN OTHER                                                       
073700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSAMTL-ATTR                  
073800           MOVE FEL-6 TO MOD-TEMFSFEL                                     
073900           MOVE NEJ TO NYCKLAR-RAETT                                      
074000       END-EVALUATE                                                       
074100                                                                          
074200       IF NYCKLAR-OK                                                      
074300         PERFORM DC-ORDNA-ALT-MOD-TILL-4694                               
074400       ELSE                                                               
074500         PERFORM S04-SAMMA-BILD                                           
074600       END-IF                                                             
074700     ELSE                                                                 
074800       PERFORM S04-SAMMA-BILD                                             
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200                                                                          
075300 DA-ALLT-VALT SECTION.                                                    
075400                                                                          
075500     MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLSAMTL-ATTR                      
075600     PERFORM DAA-DIV-KONTROLL-AV-KANTKODEN                                
075700     PERFORM S07-KOLLA-SAMMA-PRINTER                                      
075800     MOVE +1 TO RAD-LINE PTOP-LINE                                        
075900                                                                          
076000     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
076100                                                                          
076200       EVALUATE TRUE                                                      
076300         WHEN MID-KDUPPTYP(RAD-LINE) = 'E'                                
076400           CONTINUE                                                       
076500         WHEN MID-IDPRODNR (RAD-LINE) = ZERO                              
076600           CONTINUE                                                       
076700         WHEN MID-KDUPPTYP (RAD-LINE) = 'B' OR '+' OR                     
076800             (MID-KDUPPTYP (RAD-LINE) = ' ' AND                           
076900              MID-IDPRODNR (RAD-LINE) > ZERO)                             
077000           PERFORM S06-FLYTTA-RAD-TILL-PTOP                               
077100         WHEN OTHER                                                       
077200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUPPTYP-ATTR (RAD-LINE)        
077300           MOVE FEL-9              TO MOD-TEMFSFEL                        
077400           MOVE NEJ                TO NYCKLAR-RAETT                       
077500       END-EVALUATE                                                       
077600                                                                          
077700       ADD +1 TO RAD-LINE                                                 
077800     END-PERFORM                                                          
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078200 DAA-DIV-KONTROLL-AV-KANTKODEN        SECTION.                            
078300                                                                          
078400     MOVE +1                        TO IX                                 
078500                                                                          
078600     PERFORM UNTIL IX = MAX-LINE-PLUS-1                                   
078700       IF MID-SPAR-IDPRODNR-SAMP NUMERIC  AND                             
078800          MID-SPAR-IDPRODNR-SAMP = MID-IDPRODNR (IX)                      
078900         MOVE 'E'                   TO MID-KDUPPTYP (IX)                  
079000       END-IF                                                             
079100                                                                          
079200       ADD +1                       TO IX                                 
079300     END-PERFORM                                                          
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700 DB-DELVIS-VAL SECTION.                                                   
079800                                                                          
079900     MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLSAMTL-ATTR                   
080000     PERFORM DBA-DIV-KONTROLL-AV-KANTKODEN                                
080100     MOVE +1  TO RAD-LINE PTOP-LINE                                       
080200                                                                          
080300     IF NYCKLAR-OK                                                        
080400       PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                           
080500         EVALUATE TRUE                                                    
080600           WHEN MID-KDUPPTYP(RAD-LINE) = '+' OR ' '                       
080700             CONTINUE                                                     
080800           WHEN MID-KDUPPTYP(RAD-LINE) = 'V' OR 'B' OR 'K'                
080900             MOVE MFS-ALFA-FAELT-RAETT           TO                       
081000                  MOD-KDUPPTYP-ATTR(RAD-LINE)                             
081100                                                                          
081200             IF MID-KDUPPTYP(RAD-LINE) = 'K'                              
081300               MOVE MID-IDORDNR  (RAD-LINE) TO PTOP-IDORDNR-KOMPL         
081400               MOVE MID-IDPRODNR (RAD-LINE) TO PTOP-IDPRODNR-KOMPL        
081500             END-IF                                                       
081600             IF MID-KDUPPTYP(RAD-LINE) = 'V'                              
081700               MOVE MID-KDFRAKT(RAD-LINE) TO FRAK01-KDFRAKT               
081800               MOVE MID-IDDISTR(RAD-LINE) TO TEST-IDDISTR                 
081900                                                                          
082000               PERFORM  DBB-KOLLA-FRAKTKOD                                
082100                                                                          
082200               IF (MID-KDORDKL(RAD-LINE) = 0 OR 1 OR 2 OR 3)              
082300                  IF (FRAK01-KDFRAKT21 OR FRAK01-KDFRAKT62)               
082400                     IF DIST80-FRAKTS-NORDEN                              
082500                        MOVE JA TO SKRIVARE-NORDENKONTORET                
082600                     ELSE                                                 
082700                        MOVE JA TO SKRIVARE-SVERIGE2                      
082800                     END-IF                                               
082900                  ELSE                                                    
083000                     IF FRAK01-NORDEN                                     
083100                        MOVE JA TO SKRIVARE-NORDENKONTORET                
083200                     ELSE                                                 
083300                        MOVE JA TO SKRIVARE-SVERIGE2                      
083400                     END-IF                                               
083500                  END-IF                                                  
083600               END-IF                                                     
083700                                                                          
083800               IF SKRIVARE-NORDENKONTORET = JA                            
083900               AND SKRIVARE-SVERIGE2 = JA                                 
084000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSAMTL-ATTR              
084100                 MOVE FEL-13 TO MOD-TEMFSFEL                              
084200                 MOVE NEJ TO NYCKLAR-RAETT                                
084300               END-IF                                                     
084400             END-IF                                                       
084500                                                                          
084600             PERFORM S06-FLYTTA-RAD-TILL-PTOP                             
084700                                                                          
084800           WHEN OTHER                                                     
084900              MOVE FEL-8 TO MOD-TEMFSFEL                                  
085000              MOVE NEJ TO NYCKLAR-RAETT                                   
085100              MOVE MFS-ALFA-FAELT-FEL            TO                       
085200                   MOD-KDUPPTYP-ATTR(RAD-LINE)                            
085300         END-EVALUATE                                                     
085400                                                                          
085500         ADD +1 TO RAD-LINE                                               
085600       END-PERFORM                                                        
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000                                                                          
086100 DBA-DIV-KONTROLL-AV-KANTKODEN  SECTION.                                  
086200                                                                          
086300     PERFORM DBAA-KONTROLL-NR1                                            
086400                                                                          
086500     IF NYCKLAR-OK AND FLKOMPL = JA                                       
086600       PERFORM DBAB-KONTROLL-NR2                                          
086700     END-IF.                                                              
086800     EJECT                                                                
086900                                                                          
087000 DBAA-KONTROLL-NR1              SECTION.                                  
087100                                                                          
087200***************************************************************           
087300* HÄR KONTROLLERAS ATT MAN INTE ANGIVIT KANTKOD "K" FÖR TVÅ   *           
087400* ORDER SOM INTE INGÅR I SAMMA SAMPACKNING.                   *           
087500***************************************************************           
087600                                                                          
087700     MOVE NEJ TO FLKOMPL                                                  
087800     MOVE +1  TO RAD-LINE                                                 
087900                                                                          
088000     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
088100       IF MID-KDUPPTYP (RAD-LINE) = 'K'     AND                           
088200          MID-RADTEXT  (RAD-LINE) = SPACE                                 
088300         IF FLKOMPL = JA                                                  
088400           MOVE FEL-7 TO MOD-TEMFSFEL                                     
088500           MOVE NEJ TO NYCKLAR-RAETT                                      
088600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUPPTYP-ATTR(RAD-LINE)         
088700         ELSE                                                             
088800           MOVE JA                 TO FLKOMPL                             
088900         END-IF                                                           
089000       END-IF                                                             
089100                                                                          
089200       ADD +1 TO RAD-LINE                                                 
089300     END-PERFORM                                                          
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 DBAB-KONTROLL-NR2              SECTION.                                  
089800                                                                          
089900***************************************************************           
090000* HÄR KONTROLLERAS ATT MAN INTE ANGIVIT KANTKOD "K" OCH       *           
090100* KANTKOD "V" I SAMMA RELEASE.                                *           
090200***************************************************************           
090300                                                                          
090400     MOVE +1  TO RAD-LINE                                                 
090500                                                                          
090600     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
090700       IF MID-KDUPPTYP (RAD-LINE) = 'V'                                   
090800         MOVE FEL-8 TO MOD-TEMFSFEL                                       
090900         MOVE NEJ TO NYCKLAR-RAETT                                        
091000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUPPTYP-ATTR(RAD-LINE)           
091100       END-IF                                                             
091200                                                                          
091300       ADD +1 TO RAD-LINE                                                 
091400     END-PERFORM                                                          
091500     .                                                                    
091600     EJECT                                                                
091700 DBB-KOLLA-FRAKTKOD       SECTION.                                        
091800                                                                          
091900     MOVE +1  TO BILD-LINE                                                
092000     PERFORM UNTIL BILD-LINE > MAX-BILD-LINE                              
092100        IF MID-KDFRAKT  (BILD-LINE) NOT = SPACE                           
092200        AND MID-KDUPPTYP(BILD-LINE) NOT = '+'                             
092300           MOVE BILD-LINE TO JMF-IND                                      
092400           ADD +1        TO JMF-IND                                       
092500                                                                          
092600           PERFORM UNTIL JMF-IND > MAX-BILD-LINE                          
092700              IF MID-KDFRAKT (JMF-IND) NOT = SPACE                        
092800              AND MID-KDUPPTYP(JMF-IND) NOT = '+'                         
092900                                                                          
093000                 IF MID-KDFRAKT (BILD-LINE) NOT =                         
093100                    MID-KDFRAKT (JMF-IND)                                 
093200                    MOVE NEJ TO NYCKLAR-RAETT                             
093300                    MOVE FEL-15 TO MOD-TEMFSFEL                           
093400                    MOVE MFS-ALFA-FAELT-FEL TO                            
093500                         MOD-KDUPPTYP-ATTR(JMF-IND)                       
093600                 END-IF                                                   
093700              END-IF                                                      
093800              ADD +1        TO JMF-IND                                    
093900           END-PERFORM                                                    
094000        END-IF                                                            
094100        ADD +1        TO BILD-LINE                                        
094200     END-PERFORM                                                          
094300     .                                                                    
094400     SKIP2                                                                
094500 DC-ORDNA-ALT-MOD-TILL-4694     SECTION.                                  
094600                                                                          
094700     PERFORM UNTIL PTOP-LINE = MAX-LINE-PLUS-1                            
094800       MOVE QUIT           TO PTOP-RAD-KDUPPTYP (PTOP-LINE)               
094900       MOVE ZERO           TO PTOP-RAD-IDDISTR  (PTOP-LINE)               
095000                              PTOP-RAD-IDKUNDNR (PTOP-LINE)               
095100                              PTOP-RAD-KDFRAKT  (PTOP-LINE)               
095200                              PTOP-RAD-IDORDNR  (PTOP-LINE)               
095300                              PTOP-RAD-KDORDKL  (PTOP-LINE)               
095400                              PTOP-RAD-IDPRODNR (PTOP-LINE)               
095500       ADD +1              TO PTOP-LINE                                   
095600     END-PERFORM                                                          
095700                                                                          
095800     MOVE IDDISTR-WS       TO PTOP-IDDISTR-UT                             
095900     MOVE IDKUNDNR-WS      TO PTOP-IDKUNDNR-UT                            
096000     MOVE KDFRAKT-WS       TO PTOP-KDFRAKT-UT                             
096020     MOVE MSGI-IDDC        TO PTOP-IDDC-UT                                
096030     MOVE 'N'              TO PTOP-FLLANDROVER                            
096100                                                                          
096200     IF FLKOMPL = NEJ                                                     
096300       MOVE ZERO           TO PTOP-IDPRODNR-KOMPL                         
096400     END-IF                                                               
096500                                                                          
096600     MOVE JA TO INSERT-ALT.                                               
096700     EJECT                                                                
096800                                                                          
096900 DD-SATT-KANTKOD-FOR-SAMP-ORDER     SECTION.                              
097000                                                                          
097100     MOVE +1              TO RAD-LINE                                     
097200     SET TAB-LINE TO +1                                                   
097300                                                                          
097400     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
097500       IF MID-IDPRODNR (RAD-LINE) > ZERO   AND                            
097600          MID-RADTEXT  (RAD-LINE) = SPACE                                 
097700         MOVE MID-IDPRODNR (RAD-LINE)  TO                                 
097800              TAB-IDPRODNR (TAB-LINE)                                     
097900         MOVE MID-KDUPPTYP (RAD-LINE)  TO                                 
098000              TAB-KDUPPTYP (TAB-LINE)                                     
098100       ELSE                                                               
098200         MOVE 0                        TO                                 
098300              TAB-IDPRODNR (TAB-LINE)                                     
098400         MOVE LOW-VALUE                TO                                 
098500              TAB-KDUPPTYP (TAB-LINE)                                     
098600       END-IF                                                             
098700                                                                          
098800       ADD  +1                         TO RAD-LINE                        
098900       SET TAB-LINE UP BY +1                                              
099000     END-PERFORM                                                          
099100                                                                          
099200     MOVE +1              TO RAD-LINE                                     
099300                                                                          
099400     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
099500       IF MID-RADTEXT (RAD-LINE) = 'SAMP'                                 
099600         MOVE MID-IDPRODNR (RAD-LINE)  TO WE6-IDPRODNR                    
099700         PERFORM IMS-GU-WDE601                                            
099800         SET TAB-LINE TO +1                                               
099900         MOVE JA                     TO TRAFF                             
100000                                                                          
100100         SEARCH TABELL-KANTKOD                                            
100200           AT END MOVE NEJ TO TRAFF                                       
100300           WHEN VORD-IDPRODNR-SAMP = TAB-IDPRODNR (TAB-LINE)              
100400             CONTINUE                                                     
100500         END-SEARCH                                                       
100600                                                                          
100700         IF TRAFF = JA                                                    
100800           PERFORM DDA-FLYTTA-KANTK-TILL-SAMP-ORD                         
100900         ELSE                                                             
101000           PERFORM DDB-KONTROLL-OM-GODKAND-MISS                           
101100         END-IF                                                           
101200       END-IF                                                             
101300                                                                          
101400       IF MID-FLSAMTL = JA                                                
101500         PERFORM DDC-EVENTUELLT-E-MARKA-ORDER                             
101600       END-IF                                                             
101700                                                                          
101800       ADD  +1                         TO RAD-LINE                        
101900     END-PERFORM                                                          
102000     .                                                                    
102100     EJECT                                                                
102200                                                                          
102300 DDA-FLYTTA-KANTK-TILL-SAMP-ORD     SECTION.                              
102400     IF TAB-KDUPPTYP (TAB-LINE) = 'V' OR 'K' OR 'B' OR 'E'                
102500       MOVE TAB-KDUPPTYP (TAB-LINE) TO MID-KDUPPTYP (RAD-LINE)            
102600     END-IF                                                               
102700     .                                                                    
102800     SKIP3                                                                
102900                                                                          
103000 DDB-KONTROLL-OM-GODKAND-MISS     SECTION.                                
103100                                                                          
103200     EVALUATE TRUE                                                        
103300       WHEN MID-SPAR-IDPRODNR-SAMP NUMERIC  AND                           
103400            MID-SPAR-IDPRODNR-SAMP = VORD-IDPRODNR-SAMP                   
103500         CONTINUE                                                         
103600       WHEN MID-KDUPPTYP (RAD-LINE) = 'B'                                 
103700         CONTINUE                                                         
103800       WHEN OTHER                                                         
104000         MOVE FEL-14 TO MOD-TEMFSFEL                                      
104100         MOVE NEJ TO NYCKLAR-RAETT                                        
104200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUPPTYP-ATTR(RAD-LINE)           
104270         MOVE VORD-IDPRODNR-SAMP TO WS-VORD-IDPRODNR-SAMP                 
104300     END-EVALUATE                                                         
104400     .                                                                    
104500     EJECT                                                                
104600                                                                          
104700 DDC-EVENTUELLT-E-MARKA-ORDER     SECTION.                                
104800                                                                          
104900     IF MID-RADTEXT (RAD-LINE) = 'SAMP'                                   
105000       IF MID-SPAR-IDPRODNR-SAMP NUMERIC  AND                             
105100          MID-SPAR-IDPRODNR-SAMP = VORD-IDPRODNR-SAMP                     
105200         MOVE 'E'             TO MID-KDUPPTYP (RAD-LINE)                  
105300       END-IF                                                             
105400     ELSE                                                                 
105500       IF MID-SPAR-IDPRODNR-SAMP NUMERIC  AND                             
105600          MID-SPAR-IDPRODNR-SAMP = MID-IDPRODNR (RAD-LINE)                
105700         MOVE 'E'             TO MID-KDUPPTYP (RAD-LINE)                  
105800       END-IF                                                             
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200                                                                          
106300 E-RENSA-NYCKLAR SECTION.                                                 
106400                                                                          
106500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
106600                             MOD-IDKUNDNR-UT                              
106700                             MOD-KDFRAKT-UT.                              
106800     EJECT                                                                
106900                                                                          
107000 F-ENTER-GAMLA-NYCKLAR   SECTION.                                         
107100                                                                          
107200     IF MID-IDDISTR       (1) NUMERIC AND                                 
107300        MID-IDKUNDNR      (1) NUMERIC AND                                 
107400        MID-KDFRAKT       (1) NUMERIC AND                                 
107500        MID-IDPRODNR      (1) NUMERIC                                     
107600       MOVE MID-IDPRODNR  (1) TO WE6-IDPRODNR                             
107700       PERFORM IMS-GU-WDE601                                              
107800       MOVE VORD-IDPRODNR-SAMP     TO W-IDPRODNR-SAMP-MIN                 
107900       MOVE MID-IDDISTR   (1) TO W-IDDISTR-MIN                            
108000       MOVE MID-IDKUNDNR  (1) TO W-IDKUNDNR-MIN                           
108100       MOVE MID-KDFRAKT   (1) TO W-KDFRAKT-MIN                            
108200       PERFORM IMS-GU-WDE6D                                               
108300       MOVE JA                TO LAST-EN-GANG                             
108400     END-IF                                                               
108500     .                                                                    
108600     EJECT                                                                
108700                                                                          
108800 G-KONTROLL-HEL-SIDA     SECTION.                                         
108900                                                                          
109000     MOVE MID-FLSAMTL             TO MOD-FLSAMTL                          
109100                                                                          
109200     IF MID-FLSAMTL = JA OR NEJ                                           
109300       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLSAMTL-ATTR                     
109400     ELSE                                                                 
109500       MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLSAMTL-ATTR                     
109600       MOVE FEL-6                 TO MOD-TEMFSFEL                         
109700       MOVE NEJ                   TO NYCKLAR-RAETT                        
109800     END-IF                                                               
109900     .                                                                    
110000     EJECT                                                                
110100                                                                          
110200 S01-FLYTTA-HUVUD-POST SECTION.                                           
110300                                                                          
110400     MOVE VORD-IDPRODNR  TO MOD-IDPRODNR(RAD-LINE)                        
110500     MOVE VORD-IDDISTR   TO MOD-IDDISTR (RAD-LINE)                        
110600     MOVE VORD-IDKUNDNR  TO MOD-IDKUNDNR(RAD-LINE)                        
110700     MOVE VORD-KDFRAKT   TO MOD-KDFRAKT (RAD-LINE)                        
110800     MOVE VORD-KDORDKL   TO MOD-KDORDKL (RAD-LINE)                        
110900                                                                          
111000     IF VORD-IDPRODNR = VORD-IDPRODNR-SAMP                                
111100       MOVE SPACE        TO MOD-RADTEXT (RAD-LINE)                        
111200     ELSE                                                                 
111210       MOVE VORD-TIPACKN-SK TO  WS-VORD-TIPACKN-SK                        
111221       MOVE 'SAMP'       TO MOD-RADTEXT (RAD-LINE)                        
111230       IF VORD-TIPACKN-SK > 0010404                                       
111240*      OR VORD-KDFRAKT    = +069                                          
111250         MOVE JA         TO SW-VISA-PA-BILD                               
111260       END-IF                                                             
111400     END-IF                                                               
111500     .                                                                    
111600     EJECT                                                                
111700 S02-ADDERA-KOLLI SECTION.                                                
111800                                                                          
111900     IF (KOLLI-FLFRSUTS NOT = 'J' AND                                     
112000         KOLLI-KDKOLSTA >  0)                                             
112100         OR                                                               
112200        (KOLLI-FLFRSUTS NOT = 'J' AND                                     
112300         KOLLI-KDKOLSTA =  0  AND                                         
112400         KOLLI-IDKOLLI  >  99000)                                         
112600       IF KOLLI-KDKOLLI  = SPACE AND                                      
112700          KOLLI-KDEMBTYP = 0                                              
112800         CONTINUE                                                         
112900       ELSE                                                               
112910         IF  KOLLI-IDKOLLI > +149                                         
112920         AND KOLLI-IDKOLLI < +200                                         
112921         OR  KOLLI-IDKOLLI > +349                                         
112922         AND KOLLI-IDKOLLI < +400                                         
112930           CONTINUE                                                       
112940         ELSE                                                             
113000           EVALUATE KOLLI-KDEMBTYP                                        
113100             WHEN 1 ADD +1 TO WS-KVLADA-CONT                              
113200             WHEN 2 ADD +1 TO WS-KVPKT                                    
113300             WHEN 3 ADD +1 TO WS-KVBNT-STY                                
113400             WHEN 4 ADD +1 TO WS-KVHACK                                   
113500             WHEN 5 ADD +1 TO WS-KVBNT-STY                                
113600             WHEN 6 ADD +1 TO WS-KVLADA-CONT                              
113700             WHEN 7 ADD +1 TO WS-KVPALL                                   
113710             WHEN 8 ADD +1 TO WS-KVPKT                                    
113800           END-EVALUATE                                                   
113900                                                                          
114000           ADD KOLLI-VKORDBTO-KOLLI TO WS-VKORDBTO                        
114100         END-IF                                                           
114110       END-IF                                                             
114200     END-IF                                                               
114300     .                                                                    
114400     EJECT                                                                
114500 S03-FLYTTA-TILL-MOD SECTION.                                             
114520                                                                          
114530     IF  WS-KVLADA-CONT = ZERO                                            
114540     AND WS-KVPKT       = ZERO                                            
114550     AND WS-KVBNT-STY   = ZERO                                            
114560     AND WS-KVHACK      = ZERO                                            
114570     AND WS-KVPALL      = ZERO                                            
114580     AND WS-VKORDBTO    = ZERO                                            
114590     AND KORD-IDDISTR > +00099                                            
114591     AND SW-VISA-PA-BILD = 'N'                                            
114592     AND KDFRAKT-WS NOT = '69'                                            
114593       MOVE ZERO         TO MOD-IDPRODNR(RAD-LINE)                        
114594       MOVE ZERO         TO MOD-IDDISTR (RAD-LINE)                        
114595       MOVE ZERO         TO MOD-IDKUNDNR(RAD-LINE)                        
114596       MOVE ZERO         TO MOD-KDFRAKT (RAD-LINE)                        
114597       MOVE ZERO         TO MOD-KDORDKL (RAD-LINE)                        
114598       MOVE SPACE        TO MOD-RADTEXT (RAD-LINE)                        
114599       MOVE ZERO         TO MOD-IDORDNR (RAD-LINE)                        
114600       COMPUTE RAD-LINE = RAD-LINE - 1                                    
114601       END-COMPUTE                                                        
114602     ELSE                                                                 
114603       MOVE WS-KVLADA-CONT TO MOD-KVLADA-CONT (RAD-LINE)                  
114604       MOVE WS-KVPKT     TO MOD-KVPKT       (RAD-LINE)                    
114605       MOVE WS-KVBNT-STY TO MOD-KVBNT-STY   (RAD-LINE)                    
114606       MOVE WS-KVHACK    TO MOD-KVHACK      (RAD-LINE)                    
114607       MOVE WS-KVPALL    TO MOD-KVPALL      (RAD-LINE)                    
114608       MOVE WS-VKORDBTO  TO MOD-VKORDBTO    (RAD-LINE)                    
114609                                                                          
114610       MOVE ZERO         TO WS-KVLADA-CONT                                
114611                              WS-KVPKT                                    
114612                              WS-KVBNT-STY                                
114613                              WS-KVHACK                                   
114614                              WS-KVPALL                                   
114615                              WS-VKORDBTO                                 
114616     END-IF                                                               
114617     .                                                                    
114618     EJECT                                                                
116100                                                                          
116200 S04-SAMMA-BILD SECTION.                                                  
116300                                                                          
116400     MOVE +1 TO RAD-LINE                                                  
116500     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
116600        MOVE MFS-ROER-EJ-FAELT TO MOD-KDUPPTYP    (RAD-LINE)              
116700                                  MOD-IDDISTR     (RAD-LINE)              
116800                                  MOD-IDKUNDNR    (RAD-LINE)              
116900                                  MOD-KDFRAKT     (RAD-LINE)              
117000                                  MOD-IDORDNR     (RAD-LINE)              
117100                                  MOD-KDORDKL     (RAD-LINE)              
117200                                  MOD-IDPRODNR    (RAD-LINE)              
117300                                  MOD-KVLADA-CONT (RAD-LINE)              
117400                                  MOD-KVPKT       (RAD-LINE)              
117500                                  MOD-KVBNT-STY   (RAD-LINE)              
117600                                  MOD-KVHACK      (RAD-LINE)              
117700                                  MOD-KVPALL      (RAD-LINE)              
117800                                  MOD-VKORDBTO    (RAD-LINE)              
117900                                  MOD-RADTEXT     (RAD-LINE)              
118000        ADD +1 TO RAD-LINE                                                
118100     END-PERFORM                                                          
118200                                                                          
118300     MOVE MFS-ROER-EJ-FAELT    TO MOD-SPAR-IDDISTR                        
118400                                  MOD-SPAR-IDKUNDNR                       
118500                                  MOD-SPAR-KDFRAKT                        
118600                                  MOD-SPAR-IDPRODNR-SAMP.                 
118700     EJECT                                                                
118800                                                                          
118900 S05-KOLL-UPPDAT-FAELT SECTION.                                           
119000                                                                          
119100     MOVE +1 TO RAD-LINE                                                  
119200     MOVE JA TO NYCKLAR-RAETT                                             
119300     PERFORM UNTIL RAD-LINE = MAX-LINE-PLUS-1                             
119400        IF MID-KDUPPTYP (RAD-LINE) = '+' OR ' '                           
119500          CONTINUE                                                        
119600        ELSE                                                              
119700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUPPTYP-ATTR(RAD-LINE)         
119800           MOVE FEL-3 TO MOD-TEMFSFEL                                     
119900           MOVE NEJ TO NYCKLAR-RAETT                                      
120000        END-IF                                                            
120100        ADD +1 TO RAD-LINE                                                
120200     END-PERFORM                                                          
120300     .                                                                    
120400     EJECT                                                                
120500                                                                          
120600 S06-FLYTTA-RAD-TILL-PTOP     SECTION.                                    
120700     MOVE MID-KDUPPTYP        (RAD-LINE )    TO                           
120800          PTOP-RAD-KDUPPTYP   (PTOP-LINE)                                 
120900     MOVE MID-IDDISTR         (RAD-LINE )    TO                           
121000          PTOP-RAD-IDDISTR    (PTOP-LINE)                                 
121100     MOVE MID-IDKUNDNR        (RAD-LINE )    TO                           
121200          PTOP-RAD-IDKUNDNR   (PTOP-LINE)                                 
121300     MOVE MID-KDFRAKT         (RAD-LINE )    TO                           
121400          PTOP-RAD-KDFRAKT    (PTOP-LINE)                                 
121500     MOVE MID-IDORDNR         (RAD-LINE )    TO                           
121600          PTOP-RAD-IDORDNR    (PTOP-LINE)                                 
121700     MOVE MID-KDORDKL         (RAD-LINE )    TO                           
121800          PTOP-RAD-KDORDKL    (PTOP-LINE)                                 
121900     MOVE MID-IDPRODNR        (RAD-LINE )    TO                           
122000          PTOP-RAD-IDPRODNR   (PTOP-LINE)                                 
122100     ADD +1 TO PTOP-LINE.                                                 
122200     EJECT                                                                
122300 S07-KOLLA-SAMMA-PRINTER SECTION.                                         
122400     MOVE +1 TO IX                                                        
122500     PERFORM UNTIL IX = MAX-LINE-PLUS-1                                   
122600        IF MID-IDPRODNR(IX) > ZERO                                        
122700          MOVE MID-KDFRAKT(IX) TO FRAK01-KDFRAKT                          
122800          MOVE MID-IDDISTR(IX) TO TEST-IDDISTR                            
122900                                                                          
123000          IF (MID-KDORDKL(IX) = 0 OR 1 OR 2 OR 3)                         
123100             IF (FRAK01-KDFRAKT21 OR FRAK01-KDFRAKT62)                    
123200                IF DIST80-FRAKTS-NORDEN                                   
123300                   MOVE JA TO SKRIVARE-NORDENKONTORET                     
123400                ELSE                                                      
123500                   MOVE JA TO SKRIVARE-SVERIGE2                           
123600                END-IF                                                    
123700             ELSE                                                         
123800                IF FRAK01-NORDEN                                          
123900                   MOVE JA TO SKRIVARE-NORDENKONTORET                     
124000                ELSE                                                      
124100                   MOVE JA TO SKRIVARE-SVERIGE2                           
124200                END-IF                                                    
124300             END-IF                                                       
124400          END-IF                                                          
124500        END-IF                                                            
124600        ADD +1 TO IX                                                      
124700     END-PERFORM                                                          
124800     IF SKRIVARE-NORDENKONTORET = JA                                      
124900     AND SKRIVARE-SVERIGE2 = JA                                           
125000        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSAMTL-ATTR                       
125100        MOVE FEL-13 TO MOD-TEMFSFEL                                       
125200        MOVE NEJ TO NYCKLAR-RAETT                                         
125300     END-IF                                                               
125400     .                                                                    
125500* IMS SEKTIONER                                                           
125600     SKIP2                                                                
125700 IMS-GET-MSG SECTION.                                                     
125800     MOVE '  QC' TO GODK-STATUSKODER                                      
125900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     .                                                                    
126300     SKIP2                                                                
126400 IMS-INSERT-MSG SECTION.                                                  
126410     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
126420       MOVE '0' TO MFS-KDHUVOMR                                           
126430     END-IF                                                               
126800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
126900     MOVE SPACE TO GODK-STATUSKODER                                       
127000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127200     PERFORM IMS-STATUSKONTROLL.                                          
127300     SKIP2                                                                
127400 IMS-INSERT-ALT SECTION.                                                  
127500     MOVE SPACE TO GODK-STATUSKODER                                       
127600     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
127700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     EJECT                                                                
128100 IMS-GU-WDE6D SECTION.                                                    
128200     STRING 'WDE6D1  (IDDISTR  =' W-IDDISTR-MIN-X                         
128300                    '&IDKUNDNR =' W-IDKUNDNR-MIN-X                        
128400                    '&KDFRAKT  =' W-KDFRAKT-MIN-X                         
128500                    '&IDPRODNS =' W-IDPRODNR-SAMP-MIN-X                   
128600                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
128700                    '&KDORDKL <=' W-KDORDKL-MAX-X ')'                     
128800            DELIMITED BY SIZE INTO SSA1                                   
128900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
129000     CALL CBLTDLI USING GU WDE6D-PCB DLI-IO-E6D1 SSA1                     
129100     MOVE WDE6D-STATUS-CODE TO STATUS-WS                                  
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     SKIP3                                                                
129500 IMS-GN-WDE6D SECTION.                                                    
129600     STRING 'WDE6D1  (IDDISTR  =' W-IDDISTR-MIN-X                         
129700                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
129800                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
129900                    '&KDFRAKT  =' W-KDFRAKT-MIN-X                         
130000                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
130100                    '&KDORDKL <=' W-KDORDKL-MAX-X ')'                     
130200            DELIMITED BY SIZE INTO SSA1                                   
130300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130400     CALL CBLTDLI USING GN WDE6D-PCB DLI-IO-E6D1 SSA1                     
130500     MOVE WDE6D-STATUS-CODE TO STATUS-WS                                  
130600     PERFORM IMS-STATUSKONTROLL                                           
130700     .                                                                    
130800     SKIP3                                                                
130900 IMS-GN-WDE6D-MED-KNDNR SECTION.                                          
131000     STRING 'WDE6D1  (WDE6D1KY>=' WD-WDE6D1KY-MIN-X                       
131100                    '&WDE6D1KY<=' WD-WDE6D1KY-MAX-X                       
131200                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
131300                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
131400                    '&IDDC     =' W-IDDC-X ')'                            
131500            DELIMITED BY SIZE INTO SSA1                                   
131600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
131700     CALL CBLTDLI USING GN WDE6D-PCB DLI-IO-E6D1 SSA1                     
131800     MOVE WDE6D-STATUS-CODE TO STATUS-WS                                  
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100     EJECT                                                                
132200 IMS-GN-WDE6D-FRAKT SECTION.                                              
132300     STRING 'WDE6D1  (KDFRAKT  =' W-KDFRAKT-MIN-X                         
132400                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
132500                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
132600                    '&IDDC     =' W-IDDC-X ')'                            
132700            DELIMITED BY SIZE INTO SSA1                                   
132800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
132900     CALL CBLTDLI USING GN WDE6D-PCB DLI-IO-E6D1 SSA1                     
133000     MOVE WDE6D-STATUS-CODE TO STATUS-WS                                  
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     EJECT                                                                
133400 IMS-GN-WDE6D-DISTR SECTION.                                              
133500     STRING 'WDE6D1  (WDE6D1KY>=' WD-WDE6D1KY-MIN-X                       
133600                    '&WDE6D1KY<=' WD-WDE6D1KY-MAX-X                       
133700                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
133800                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
133900                    '&IDDC     =' W-IDDC-X ')'                            
134000            DELIMITED BY SIZE INTO SSA1                                   
134100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
134200     CALL CBLTDLI USING GN WDE6D-PCB DLI-IO-E6D1 SSA1                     
134300     MOVE WDE6D-STATUS-CODE TO STATUS-WS                                  
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600     EJECT                                                                
134700 IMS-GU-WDE601   SECTION.                                                 
134800     STRING 'WDE601  (IDPRODNR =' WE6-IDPRODNR-X ')'                      
134900            DELIMITED BY SIZE INTO SSA2                                   
135000     MOVE '  ' TO GODK-STATUSKODER                                        
135100     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA2                      
135200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500     SKIP2                                                                
135600 IMS-GNP-WDE611     SECTION.                                              
135700     MOVE 'WDE611 ' TO SSA1                                               
135800     MOVE '  GE' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
136000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     SKIP2                                                                
136310 IMS-GU-WDE401-ESEQ SECTION.                                              
136320     STRING 'WDE401  (WDE4ESEQ =' WE6-IDPRODNR-X ')'                      
136330            DELIMITED BY SIZE INTO SSA1                                   
136340     MOVE '    ' TO GODK-STATUSKODER                                      
136350     CALL CBLTDLI USING GU  WDE4-PCB DLI-IO-E401 SSA1                     
136360     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
136370     PERFORM IMS-STATUSKONTROLL                                           
136390     .                                                                    
136391                                                                          
136392 IMS-GU-WDB601    SECTION.                                                
136393     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
136394          DELIMITED BY SIZE INTO SSA1                                     
136395     MOVE '  GE' TO GODK-STATUSKODER                                      
136396     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
136397     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
136398     PERFORM IMS-STATUSKONTROLL                                           
136399     IF SEGMENT-SAKNAS                                                    
136400        MOVE SPACE TO DCS-KDDC                                            
136401     END-IF                                                               
136410     .                                                                    
136500     EJECT                                                                
137200 IMS-STATUSKONTROLL SECTION.                                              
137300     SET STATUS-IX TO 1                                                   
137400     SEARCH GODK-STATUS AT END CALL FELLOG                                
137500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
137600     END-SEARCH                                                           
137700     .                                                                    
