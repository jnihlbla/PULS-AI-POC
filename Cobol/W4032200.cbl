000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4032200.                                                
000400 AUTHOR.         CAP GEMINI BRA - BOH                                     
000500 DATE-WRITTEN.   JAN   85.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*    PROGRAMMET ÄR ETT FRÅGEPROGRAM                                       
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W4T322                                              
001300*        MID:         W4I32201                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W4O32201                                            
001700*                                                                         
001800* CHANGE LOG:                                                             
001900*                                                                         
002000* DIGAMBAR/020516                                                         
002100* READ THE WDE8: LDC CUSTOMER STEERING                                    
002200*                                                                         
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000*    -- CHECKED BY WY2000                                                 
003100     SKIP3                                                                
003200 77   PROGRAM-NAMN           VALUE 'W4032200'                             
003300                                 PIC X(8).                                
003400 77  JA                          PIC X(1)    VALUE 'J'.                   
003500 77  YES                         PIC X(1)    VALUE 'J'.                   
003600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003700 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
003800 77  INDX                        PIC S9(4)   COMP SYNC.                   
003900 77  FRAKTTEXT-INDX              PIC 9(2).                                
004000 77  WS-IDDISTR                  PIC X(4).                                
004100 77  W-IDDISTR-NUM               PIC 9(4).                                
004200 77  WS-IDKUNDNR                 PIC X(6).                                
004300 77  WS-IDPRODNR                 PIC X(7).                                
004400 77  WS-IDPLKLST                 PIC S9(3).                               
004500 77  WS-JFR-IDPRODNR             PIC X(7).                                
004600 77  WS-IDORDER                  PIC S9(7).                               
004700 77  WS-IDTIDZON                 PIC X(2).                                
004800 77  WS-KDFRAKT                  PIC S9(3)   COMP-3.                      
004900 77  WS-KDPERSON                 PIC S9(3)   COMP-3.                      
005000 77  WS-KDMFSFOR                 PIC 9.                                   
005100 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3 VALUE ZERO.           
005200 77  MOD-LAENGD                  PIC S9(4)   VALUE +507 COMP SYNC.        
005300 77  WS-TILOKDAT                 PIC 9(7).                                
005400 77  WS-VKORDNTO                 PIC S9(6)V9.                             
005500 77  WS-VLORDNTO                 PIC 9(4)V9(3)  VALUE ZERO.               
005600                                                                          
005700 01  WS-KDMATT                   PIC X.                                   
005800     88 US-MEASUREMENT           VALUE 'U'.                               
005900     88 SIS-MEASUREMENT          VALUE 'S'.                               
006000                                                                          
006100 01 WS-TILOKTID-NUM8.                                                     
006200     03 WS-TILOKTID-NUM6      PIC 9(6).                                   
006300     03 WS-ZERO               PIC 99.                                     
006400*                                                                         
006500 01  WS-ADGMT-GATA-SATS.                                                  
006600     03  FILLER                  PIC X(19).                               
006700     03  WS-KORD-IDARTNR-SATS    PIC X(08).                               
006800*                                                                         
006900 01  WS-ANT-RAD-ART-SATS.                                                 
007000     03  FILLER                  PIC X(18).                               
007100     03  WS-KORD-KVORDRAD        PIC X(03).                               
007200     03  WS-KORD-KVBEART-SATS    PIC X(06).                               
007300*                                                                         
007400 77  WS-SLINGA-KLAR              PIC X(1).                                
007500     88  SLINGA-KLAR                         VALUE 'J'.                   
007600*                                                                         
007700 77  WS-IDTRANS                  PIC X(4).                                
007800     88  WS-GODKAEND-BILD                    VALUE '4321' '4322'          
007900                                            '4323' '4324' '4325'.         
008000     88  EGEN-MID                            VALUE '4322'.                
008100*                                                                         
008200 01  WS-IDKUNDRF.                                                         
008300     03  WS-IDORDNR              PIC X(5).                                
008400     03  FILLER                  PIC X(5)  VALUE SPACE.                   
008500*                                                                         
008600 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
008700                                                                          
008800     88  FEL-FUNNET                        VALUE 'J'.                     
008900     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
009000                                                                          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200                                                                          
009300     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
009400     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
009500     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
009600     03  WWOMVAND                PIC X(8)  VALUE 'WWOMVAND'.              
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009900 01    FILLER                    PIC X(16)   VALUE 'WMSGINIT '.           
010000*01 -COPY WMSGINIT                                                        
010100     SKIP3                                                                
010200 01    FILLER                    PIC X(16)   VALUE 'WWOMVAND '.           
010300*01    -COPY WWOMVAND                                                     
010400     SKIP2                                                                
010500 01    FILLER                    PIC X(16)   VALUE 'WWDIST19'.            
010600*01    -COPY WWDIST19                                                     
010700     SKIP2                                                                
010800 01    FILLER                    PIC X(17)                                
010900                                 VALUE 'PARAMETER FOR S10'.               
011000 77  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
011100 77  TEST-IDKUNDNR               PIC 9(7)    COMP-3.                      
011200                                                                          
011300 01  W-SPAR-IDKUNDRF.                                                     
011400     03  FILLER                  PIC X(2)    VALUE '00'.                  
011500     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
011600     03  FILLER                  PIC X(3)    VALUE '+++'.                 
011700 01  WS-BEGMRK.                                                           
011800                                                                          
011900     03  WS-BEGMRK-1           PIC X(30).                                 
012000     03  WS-BEGMRK-2           PIC X(30).                                 
012100     SKIP2                                                                
012200 01  WS-VAGNNR.                                                           
012300                                                                          
012400     03  WS-IDPRC                PIC X(4).                                
012500     03  FILLER                  PIC X     VALUE '-'.                     
012600     03  WS-IDLOTNR              PIC 9(3).                                
012700     SKIP2                                                                
012800 01  WS-TIUTSKR-NUM              PIC 9(7).                                
012900 01  WS-TIUTSTID-NUM             PIC 9(7).                                
013000     SKIP2                                                                
013100 01  WS-UTSKRIFTSDATUM.                                                   
013200                                                                          
013300     03  WS-TIUTSKR              PIC X(6).                                
013400     03  FILLER                  PIC X     VALUE '-'.                     
013500     03  WS-TIUTSTID             PIC X(6).                                
013600     SKIP2                                                                
013700 01  WS-PRODNR-AER-NYCKEL        PIC X     VALUE 'J'.                     
013800                                                                          
013900     88  PRODNR-AER-NYCKEL                 VALUE 'J'.                     
014000     EJECT                                                                
014100 01  NYCKLAR-TILL-DLI.                                                    
014200                                                                          
014300   03    W-WDE4E1KY-X.                                                    
014400     05    W-IDPRODNR-WDE4E      PIC S9(7)   VALUE ZERO  COMP-3.          
014500                                                                          
014600     03  W-E601-IDPRODNR-X.                                               
014700         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
014800                                                                          
014900     03  W-E4A1-WDE4KEY-X.                                                
015000         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
015100         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
015200         05  W-E4A1-IDKUNDRF.                                             
015300             07  W-E4A1-IDORDNR  PIC X(5).                                
015400             07  FILLER          PIC X(5)    VALUE SPACE.                 
015500                                                                          
015600     03  W-E401-WDE4KEY-X.                                                
015700         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
015800         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
015900         05  W-E401-IDKUNDRF.                                             
016000             07  W-E401-IDORDNR  PIC X(5).                                
016100             07  FILLER          PIC X(5)    VALUE SPACE.                 
016200         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
016300         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
016400                                                                          
016500     03  W-WDB101KY-X.                                                    
016600         05  W-IDPARTNR          PIC  X(9).                               
016700         05  W-IDFTG             PIC  9(2).                               
016800                                                                          
016900     03  W-IDGMT-X.                                                       
017000         05  W-IDDISTR           PIC S9(5)  COMP-3.                       
017100         05  W-IDKUNDNR          PIC S9(7)  COMP-3.                       
017200                                                                          
017300     03  W-473B-WDGXKEY-X.                                                
017400         05  W-473B-IDHTYP       PIC X(4)    VALUE '4732'.                
017500         05  W-473B-KDFRAKT      PIC S9(3)   COMP-3.                      
017600         05  W-473B-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
017700                                                                          
017800     03  W-4732-KDSEGKEY-X.                                               
017900         05  W-4732-KDSEGKEY     PIC X       VALUE '1'.                   
018000                                                                          
018100     03  W-0801-WDGXKEY-X.                                                
018200         05  W-0801-IDHTYP       PIC X(4)    VALUE '0801'.                
018300         05  W-0801-KDARBTYP     PIC X(8)    VALUE 'CDC     '.            
018400         05  W-0801-IDKEYREST    PIC X(18)   VALUE LOW-VALUE.             
018500                                                                          
018600     03  W-0802-IDPERSON-X.                                               
018700         05  W-0802-IDPERSON     PIC S9(3)   COMP-3 VALUE +0.             
018800*                                                                         
018900     03  W-WDQ2CSEQ-X.                                                    
019000       05  W-WDQ2C-IDGMTREF-X.                                            
019100         07  W-WDQ2C-IDDISTR     PIC S9(5)   COMP-3 VALUE +0.             
019200         07  W-WDQ2C-IDKUNDNR    PIC S9(7)   COMP-3 VALUE +0.             
019300         07  W-WDQ2C-IDKUNDRF.                                            
019400           09  FILLER            PIC  9(2)          VALUE ZERO.           
019500           09  W-WDQ2C-IDORDNR5                                           
019600                                 PIC  9(5)          VALUE ZERO.           
019700           09  FILLER            PIC  X(3)          VALUE SPACE.          
019800*                                                                         
019900     03  W-WDQ301KY-X.                                                    
020000         05  W-Q301-IDORDER      PIC S9(7)    COMP-3.                     
020100         05  W-Q301-IDDC         PIC  X(2).                               
020200         05  W-Q301-IDPRODNR     PIC S9(7)    COMP-3.                     
020300         05  W-Q301-IDPLKLST     PIC S9(3)    COMP-3.                     
020400*                                                                         
020500     03  W-IDDC-B6-X.                                                     
020600         05 W-IDDC-B6            PIC X(2).                                
020700                                                                          
020800 01  MEDDELANDE.                                                          
020900                                                                          
021000     03  FEL-1.                                                           
021100         05  FILLER              PIC X(40)   VALUE                        
021200             '701 ORDERN SAKNAS                       '.                  
021300         05  FILLER              PIC X(40)   VALUE                        
021400             '701 ORDER MISSING                       '.                  
021500     03  FEL-701 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
021600                                                                          
021700     03  FEL-2.                                                           
021800         05  FILLER              PIC X(40)   VALUE                        
021900             '749 FEL NYCKEL                          '.                  
022000         05  FILLER              PIC X(40)   VALUE                        
022100             '749 WRONG KEY                           '.                  
022200     03  FEL-749 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
022300     EJECT                                                                
022400 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
022500                                                                          
022600 01    FILLER              PIC X(16)   VALUE 'MID W4I322 MID'.            
022700*01  MID -COPY W4I32201.                                                  
022800     EJECT                                                                
022900*01  -COPY WMSGAREA                                                       
023000     EJECT                                                                
023100*    03  MOD -COPY W4O32201  -RED MSG-AREA.                               
023200     EJECT                                                                
023300*01  -COPY WMFSAREA                                                       
023400     EJECT                                                                
023500 01  IMS-WS.                                                              
023600     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
023700     SKIP3                                                                
023800*                        **** STATUS-KOD FRÅN IMS                         
023900     03  STATUS-WS               PIC X(2).                                
024000         88  SEGMENT-FINNS                   VALUE '  '.                  
024100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
024200     03  STATUS-WDE401-SEK-WS    PIC X(2).                                
024300         88  WDE401-SEK-FINNS                VALUE '  '.                  
024400         88  WDE401-SEK-SAKNAS               VALUE 'GE' 'GB'.             
024500     SKIP3                                                                
024600     03  GODK-STATUSKODER.                                                
024700         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
024800     SKIP3                                                                
024900 01  SSA1                        PIC X(64).                               
025000 01  SSA2                        PIC X(64).                               
025100     EJECT                                                                
025200*                            IMS FUNKTIONSKODER                           
025300*01  -COPY W0003                                                          
025400     EJECT                                                                
025500*                            DLI INPUT-OUTPUT AREA                        
025600 01  DLI-IO-AREA.                                                         
025700     03  IO-AREA                 PIC X(652)  VALUE SPACE.                 
025800     SKIP2                                                                
025900*    03  WDE601   -COPY WDE601     -RED IO-AREA.                          
026000     EJECT                                                                
026100*    03  WDE401   -COPY WDE401     -RED IO-AREA.                          
026200     EJECT                                                                
026300*    03  WL473211 -COPY WDGX4732   -RED IO-AREA.                          
026400     EJECT                                                                
026500*    03  WDQ201   -COPY WDQ201     -RED IO-AREA.                          
026600     EJECT                                                                
026700 01  DLI-IO-WDE401.                                                       
026800*    03        -COPY WDE401    -PRE E4E-                                  
026900     EJECT                                                                
027000 01  FILLER                      PIC X(16)  VALUE 'WDB1-AREA'.            
027100*01  -COPY WDB101                                                         
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
027400 01  DLI-IO-AREA-WDB2.                                                    
027500*    03  -COPY WDB201                                                     
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)  VALUE 'WDQ301-AREA'.          
027800*01  -COPY WDQ301                                                         
027900     EJECT                                                                
028000                                                                          
028100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028200 01   DLI-IO-AREA-B601.                                                   
028300*     03  -COPY WDB601                                                    
028400                                                                          
028500 LINKAGE SECTION.                                                         
028600*01  -COPY W0009     -PRE MSG-                                            
028700     EJECT                                                                
028800*01  -COPY W0008     -PRE USEA-                                           
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008     -PRE WDE62-                                          
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE 4732-                                           
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008     -PRE WDE4-                                           
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008     -PRE WDE4E-                                          
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008     -PRE WDE42-                                          
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008     -PRE WDE6-                                           
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008     -PRE ORQICSQ-                                        
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008     -PRE BETC-                                           
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008     -PRE GMTA-                                           
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008     -PRE ORQA-                                           
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008     -PRE WDB6-                                           
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB    WDE4E-PCB                   
032500                                  WDE62-PCB   4732-PCB  WDE4-PCB          
032600                                  WDE42-PCB   WDE6-PCB ORQICSQ-PCB        
032700                                  BETC-PCB    GMTA-PCB  ORQA-PCB          
032800                                  WDB6-PCB.                               
032900     SKIP2                                                                
033000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB  WDE4E-PCB                    
033100                                   WDE62-PCB 4732-PCB WDE4-PCB            
033200                                   WDE42-PCB WDE6-PCB ORQICSQ-PCB         
033300                                   BETC-PCB  GMTA-PCB ORQA-PCB            
033400                                   WDB6-PCB.                              
033500     SKIP2                                                                
033600 STYR SECTION.                                                            
033700     PERFORM IMS-GET-MSG                                                  
033800                                                                          
033900     IF SEGMENT-FINNS                                                     
034000         PERFORM A-INIT-SPARA-INPUT                                       
034100         IF WS-GODKAEND-BILD                                              
034200         PERFORM B-VILKA-NYCKLAR-ANVAENDS                                 
034300                                                                          
034400         IF PRODNR-AER-NYCKEL                                             
034500             IF WS-IDPRODNR NUMERIC AND                                   
034600                FEL-EJ-FUNNET                                             
034700                 PERFORM C-LAES-MED-PRODNR                                
034800             ELSE                                                         
034900                 MOVE FEL-749(INDX)   TO MOD-TEMFSFEL                     
035000                 IF NOT WS-GODKAEND-BILD                                  
035100                     PERFORM E-RENSA-NYCKLAR                              
035200                 END-IF                                                   
035300             END-IF                                                       
035400         ELSE                                                             
035500             IF  WS-IDDISTR NUMERIC                                       
035600             AND WS-IDKUNDNR NUMERIC                                      
035700             AND WS-IDORDNR  NUMERIC                                      
035800             AND FEL-EJ-FUNNET                                            
035900                 PERFORM D-LAES-MED-DISTR-KUND-ORDER                      
036000             ELSE                                                         
036100                 MOVE FEL-749(INDX)   TO MOD-TEMFSFEL                     
036200                 IF NOT WS-GODKAEND-BILD                                  
036300                     PERFORM E-RENSA-NYCKLAR                              
036400                 END-IF                                                   
036500             END-IF                                                       
036600         END-IF                                                           
036700         END-IF                                                           
036800         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O32201 + 4                    
036900         PERFORM   IMS-INSERT-MSG                                         
037000     END-IF                                                               
037100     MOVE ZERO TO RETURN-CODE                                             
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT-SPARA-INPUT SECTION.                                              
037600                                                                          
037700     IF MSG-DUBBLA-TRANSKODER                                             
037800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I32201               
037900         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
038000         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
038100         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
038200         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
038300     ELSE                                                                 
038400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I32201                
038500         MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                 
038600         MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                
038700         MOVE ' '                          TO MFS-KDTRTYP                 
038800     END-IF                                                               
038900                                                                          
039000     MOVE JA                 TO WS-PRODNR-AER-NYCKEL                      
039100     MOVE NEJ                TO WS-FEL-FUNNET                             
039200     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
039300                                                                          
039400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039500     MOVE '001'             TO MSGI-KDCALL                                
039600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039700     MOVE '4322'            TO MSGI-IDTRANS                               
039800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039900     IF EGEN-MID                                                          
040000        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
040100        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
040200        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
040300        IF MID-IDORDNR-IN       NOT = ALL '+'                             
040400           MOVE MID-IDORDNR-IN  TO W-SPAR-IDORDNR5                        
040500           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
040600        END-IF                                                            
040700     END-IF                                                               
040800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040900                                                                          
041000     MOVE MSGI-KDMATT        TO WS-KDMATT                                 
041100     MOVE MSGI-IDTIDZON      TO WS-IDTIDZON                               
041200                                                                          
041300     IF MSGI-IDLAND-SPR = 'GB'                                            
041400       MOVE +2 TO INDX                                                    
041500     ELSE                                                                 
041600       MOVE +1 TO INDX                                                    
041700     END-IF                                                               
041800                                                                          
041900     IF WS-GODKAEND-BILD                                                  
042000        IF MID-IDDISTR-IN = ALL '+'                                       
042100            MOVE MID-IDDISTR-UT TO WS-IDDISTR                             
042200            INSPECT WS-IDDISTR REPLACING ALL SPACE BY ZERO                
042300        ELSE                                                              
042400            MOVE MID-IDDISTR-IN TO WS-IDDISTR                             
042500        END-IF                                                            
042600                                                                          
042700        IF MID-IDKUNDNR-IN = ALL '+'                                      
042800            MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                           
042900            INSPECT WS-IDKUNDNR REPLACING ALL SPACE BY ZERO               
043000        ELSE                                                              
043100            MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                           
043200        END-IF                                                            
043300                                                                          
043400        IF MID-IDORDNR-IN    = ALL '+'                                    
043500            MOVE MID-IDORDNR-UT TO WS-IDORDNR                             
043600            INSPECT WS-IDORDNR REPLACING ALL SPACE BY ZERO                
043700        ELSE                                                              
043800            MOVE MID-IDORDNR-IN TO WS-IDORDNR                             
043900        END-IF                                                            
044000     END-IF                                                               
044100                                                                          
044200     IF WS-GODKAEND-BILD                                                  
044300        IF MID-IDPRODNR-IN = ALL '+'                                      
044400            MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                           
044500            INSPECT WS-IDPRODNR REPLACING ALL SPACE BY ZERO               
044600        ELSE                                                              
044700            MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                           
044800        END-IF                                                            
044900                                                                          
045000       IF MID-IDDC-IN = ALL '+'                                           
045100         MOVE MID-IDDC-UT              TO WS-IDDC                         
045200       ELSE                                                               
045300         MOVE MID-IDDC-IN              TO WS-IDDC                         
045400       END-IF                                                             
045500     ELSE                                                                 
045600        MOVE MSGI-IDPRODNR       TO WS-IDPRODNR                           
045700        MOVE MSGI-IDDC                 TO WS-IDDC                         
045800     END-IF                                                               
045900                                                                          
046000     IF WS-IDDC = SPACE                                                   
046100        MOVE MSGI-IDDC                 TO WS-IDDC                         
046200     END-IF                                                               
046300                                                                          
046400     MOVE LOW-VALUE      TO MSG-AREA                                      
046500     MOVE 'W4O322N1'     TO MFS-IDMOD                                     
046600     MOVE '4322'         TO MOD-IDTRANS                                   
046700                                                                          
046800     MOVE WS-IDDISTR     TO MOD-IDDISTR-UT                                
046900     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
047000                                                                          
047100     MOVE WS-IDKUNDNR    TO MOD-IDKUNDNR-UT                               
047200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
047300                                                                          
047400     MOVE WS-IDORDNR     TO MOD-IDORDNR-UT                                
047500     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
047600                                                                          
047700     MOVE WS-IDPRODNR    TO MOD-IDPRODNR-UT                               
047800     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
047900                                                                          
048000     MOVE WS-IDDC         TO MOD-IDDC-UT                                  
048100                                                                          
048200     MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-IN                              
048300                             MOD-IDANSTNR-UT                              
048400                             MOD-IDDISTR-IN                               
048500                             MOD-IDKUNDNR-IN                              
048600                             MOD-IDORDNR-IN                               
048700                             MOD-IDPRODNR-IN                              
048800                             MOD-IDKOLLI-IN                               
048900                             MOD-IDKOLLI-UT                               
049000                             MOD-IDDC-IN                                  
049100                             MOD-IDDISTR                                  
049200                             MOD-IDKUNDNR                                 
049300                             MOD-IDDC                                     
049400                             MOD-KDFRAKT                                  
049500                             MOD-IDKUNDRF                                 
049600                             MOD-BEGMT-RAD1                               
049700                             MOD-BEGMT-RAD2                               
049800                             MOD-ADGMT-GATA                               
049900                             MOD-ADGMT-PADR                               
050000                             MOD-ADGMT-LAND                               
050100                             MOD-UTSKRIFTSDATUM                           
050200                             MOD-BEVARREF                                 
050300                             MOD-TIORDREG                                 
050400                             MOD-KDORDKL                                  
050500                             MOD-TIBEGPAC                                 
050600                             MOD-TIPACKN-SK                               
050700                             MOD-BEFRAKT                                  
050800                             MOD-VAGNNR                                   
050900                             MOD-IDUSER                                   
051000                             MOD-BETELNR                                  
051100                             MOD-VLORDNTO                                 
051200                             MOD-VKORDNTO                                 
051300                             MOD-KVORDRAD                                 
051400                             MOD-BEGMRK-1                                 
051500                             MOD-BEGMRK-2                                 
051600                             MOD-BELAGINS-DEL1                            
051700                             MOD-BELAGINS-DEL2                            
051800                             MOD-LAGERAVBOK                               
051900                             MOD-FLLSBOK                                  
052000                             MOD-TEMFSFEL                                 
052100                             MOD-TEMFSINF                                 
052200     .                                                                    
052300     EJECT                                                                
052400 B-VILKA-NYCKLAR-ANVAENDS SECTION.                                        
052500                                                                          
052600     IF WS-GODKAEND-BILD                                                  
052700        IF MID-IDPRODNR-IN NOT = ALL '+'                                  
052800                                                                          
052900            MOVE JA TO WS-PRODNR-AER-NYCKEL                               
053000            MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                        
053100                                    MOD-IDKUNDNR-UT                       
053200                                    MOD-IDORDNR-UT                        
053300                                                                          
053400        ELSE                                                              
053500                                                                          
053600           IF MID-IDDISTR-IN = ALL '+'                                    
053700           AND MID-IDKUNDNR-IN = ALL '+'                                  
053800           AND MID-IDORDNR-IN = ALL '+'                                   
053900                                                                          
054000              IF WS-IDPRODNR NUMERIC                                      
054100              AND WS-IDPRODNR > ZERO                                      
054200                 MOVE JA     TO WS-PRODNR-AER-NYCKEL                      
054300                 MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                   
054400                                         MOD-IDKUNDNR-UT                  
054500                                         MOD-IDORDNR-UT                   
054600              ELSE                                                        
054700                 MOVE NEJ TO WS-PRODNR-AER-NYCKEL                         
054800                 MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                  
054900              END-IF                                                      
055000           ELSE                                                           
055100              MOVE NEJ TO WS-PRODNR-AER-NYCKEL                            
055200              MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                     
055300           END-IF                                                         
055400        END-IF                                                            
055500     ELSE                                                                 
055600       MOVE JA TO WS-PRODNR-AER-NYCKEL                                    
055700       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
055800                               MOD-IDKUNDNR-UT                            
055900                               MOD-IDORDNR-UT                             
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 C-LAES-MED-PRODNR SECTION.                                               
056400     MOVE WS-IDPRODNR       TO W-E601-IDPRODNR                            
056500     PERFORM IMS-GU-WDE601-KVAL                                           
056600     IF SEGMENT-FINNS                                                     
056700         PERFORM S01-DATA-FRAAN-WDE601                                    
056800         MOVE VORD-IDPRODNR      TO W-IDPRODNR-WDE4E                      
056900         PERFORM IMS-GU-WDE401-ESEQ                                       
057000*        PERFORM S02-DATA-FRAAN-WDE401                                    
057100         MOVE E4E-KORD-IDKUNDRF TO MOD-IDKUNDRF                           
057200         MOVE E4E-KORD-TIORDREG TO MOD-TIORDREG                           
057300         IF E4E-KORD-FLLSBOK = NEJ                                        
057400           IF ENGLISH-TEXT                                                
057500             MOVE '   STOCKUPD.' TO MOD-LAGERAVBOK                        
057600           ELSE                                                           
057700             MOVE 'LAGERAVBOKN.' TO MOD-LAGERAVBOK                        
057800           END-IF                                                         
057900           MOVE NEJ              TO MOD-FLLSBOK                           
058000         ELSE                                                             
058100           MOVE MFS-RENSA-FAELT  TO MOD-LAGERAVBOK                        
058200                                MOD-FLLSBOK                               
058300         END-IF                                                           
058400         MOVE E4E-KORD-IDDISTR TO W-E401-IDDISTR                          
058500                                  W-IDDISTR                               
058600                                  W-WDQ2C-IDDISTR                         
058700         MOVE E4E-KORD-IDKUNDNR TO W-E401-IDKUNDNR                        
058800                                  W-IDKUNDNR                              
058900                                  W-WDQ2C-IDKUNDNR                        
059000         MOVE E4E-KORD-IDKUNDRF TO W-E401-IDKUNDRF                        
059100         MOVE E4E-KORD-IDKUNDRF(1:5)                                      
059200                               TO W-WDQ2C-IDORDNR5                        
059300         MOVE E4E-KORD-IDPRODNR TO W-E401-IDPRODNR                        
059400         MOVE E4E-KORD-IDPLKLST TO W-E401-IDPLKLST                        
059500                                  WS-IDPLKLST                             
059600         MOVE E4E-KORD-IDORDER TO WS-IDORDER                              
059700         PERFORM IMS-GET-ORQI01-CSEQ                                      
059800         IF SEGMENT-FINNS                                                 
059900* FIX - WS-IDDISTR WAS NOT PASSED CORRECTLY  FOR S08                      
060000           MOVE    OHUV-IDDISTR    TO W-IDDISTR-NUM                       
060100           MOVE    W-IDDISTR-NUM  TO WS-IDDISTR                           
060200           PERFORM S08-HAMTA-IDUSER-FRAN-Q201                             
060300         END-IF                                                           
060400         PERFORM S05-BEHANDLA-HAENDELSEBASER                              
060500         PERFORM S07-GET-WDQ301-INFO                                      
060600     ELSE                                                                 
060700         MOVE FEL-701(INDX)  TO MOD-TEMFSFEL                              
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 D-LAES-MED-DISTR-KUND-ORDER SECTION.                                     
061200     MOVE 'N'             TO WS-SLINGA-KLAR                               
061300                                                                          
061400     MOVE WS-IDDISTR      TO W-E4A1-IDDISTR                               
061500                             W-IDDISTR                                    
061600     MOVE WS-IDKUNDNR     TO W-E4A1-IDKUNDNR                              
061700                             W-IDKUNDNR                                   
061800     MOVE WS-IDORDNR      TO W-E4A1-IDORDNR                               
061900     PERFORM IMS-GU-WDE401-SEK                                            
062000                                                                          
062100     IF WDE401-SEK-FINNS                                                  
062200        PERFORM UNTIL WDE401-SEK-SAKNAS OR                                
062300                      SLINGA-KLAR                                         
062400           MOVE KORD-IDDISTR       TO W-E401-IDDISTR                      
062500                                      W-WDQ2C-IDDISTR                     
062600           MOVE KORD-IDKUNDNR      TO W-E401-IDKUNDNR                     
062700                                      W-WDQ2C-IDKUNDNR                    
062800           MOVE KORD-IDORDNR5      TO W-E401-IDORDNR                      
062900                                      W-WDQ2C-IDORDNR5                    
063000           MOVE KORD-IDPRODNR      TO W-E401-IDPRODNR                     
063100           MOVE KORD-IDORDER       TO WS-IDORDER                          
063200           MOVE KORD-IDPLKLST      TO W-E401-IDPLKLST                     
063300                                      WS-IDPLKLST                         
063400           PERFORM IMS-GET-ORQI01-CSEQ                                    
063500           IF SEGMENT-FINNS                                               
063600             PERFORM S08-HAMTA-IDUSER-FRAN-Q201                           
063700           END-IF                                                         
063800           PERFORM IMS-GU-WDE401-KVAL                                     
063900           MOVE KORD-KVORDRAD-LEVPL   TO WS-SPAR-KVORDRAD-LEVPL           
064000           PERFORM S02-DATA-FRAAN-WDE401                                  
064100           MOVE KORD-IDPRODNR         TO W-E601-IDPRODNR                  
064200           PERFORM IMS-GU-WDE601-OKVAL                                    
064300           MOVE MFS-KDMFSFOR TO WS-KDMFSFOR                               
064400           IF SEGMENT-FINNS                                               
064500              IF PRODNR-AER-NYCKEL                                        
064600                 MOVE VORD-IDPRODNR TO WS-JFR-IDPRODNR                    
064700                 IF WS-JFR-IDPRODNR = WS-IDPRODNR                         
064800                    CONTINUE                                              
064900                 ELSE                                                     
065000                    SET SEGMENT-SAKNAS TO TRUE                            
065100                 END-IF                                                   
065200              ELSE                                                        
065300                 IF VORD-IDDC = WS-IDDC AND                               
065400                    WS-SPAR-KVORDRAD-LEVPL = ZERO                         
065500                    CONTINUE                                              
065600                 ELSE                                                     
065700                    SET SEGMENT-SAKNAS TO TRUE                            
065800                 END-IF                                                   
065900              END-IF                                                      
066000           END-IF                                                         
066100           IF SEGMENT-FINNS                                               
066200              MOVE 'J'             TO WS-SLINGA-KLAR                      
066300              PERFORM S01-DATA-FRAAN-WDE601                               
066400              PERFORM S05-BEHANDLA-HAENDELSEBASER                         
066500              PERFORM S07-GET-WDQ301-INFO                                 
066600           END-IF                                                         
066700           PERFORM IMS-GN-WDE401-SEK                                      
066800        END-PERFORM                                                       
066900     ELSE                                                                 
067000        MOVE FEL-701(INDX)   TO MOD-TEMFSFEL                              
067100     END-IF                                                               
067200*                                                                         
067300     IF NOT SLINGA-KLAR                                                   
067400        MOVE FEL-701(INDX)   TO MOD-TEMFSFEL                              
067500     END-IF                                                               
067600     .                                                                    
067700     SKIP2                                                                
067800 E-RENSA-NYCKLAR SECTION.                                                 
067900     MOVE MFS-RENSA-FAELT              TO  MOD-IDDISTR-UT                 
068000                                           MOD-IDKUNDNR-UT                
068100                                           MOD-IDORDNR-UT                 
068200                                           MOD-IDKOLLI-UT                 
068300                                           MOD-IDPRODNR-UT                
068400                                           MOD-IDDC-UT                    
068500     .                                                                    
068600     SKIP2                                                                
068700 S01-DATA-FRAAN-WDE601       SECTION.                                     
068800                                                                          
068900     MOVE VORD-KDFRAKT       TO WS-KDFRAKT                                
069000                                MOD-KDFRAKT                               
069100     MOVE VORD-IDDC          TO MOD-IDDC                                  
069200*    MOVE VORD-KDPERSON      TO WS-KDPERSON                               
069300     MOVE VORD-IDDISTR       TO MOD-IDDISTR                               
069400     MOVE VORD-IDKUNDNR      TO MOD-IDKUNDNR                              
069500     MOVE VORD-KDORDKL       TO MOD-KDORDKL                               
069600     MOVE VORD-DABEGPAC (3:6)  TO MOD-TIBEGPAC                            
069700                                                                          
069800                                                                          
069900     MOVE VORD-TIUTSKR         TO WS-TIUTSKR-NUM                          
070000     MOVE WS-TIUTSKR-NUM(2:6)  TO WS-TIUTSKR                              
070100     MOVE VORD-TIUTSTID        TO WS-TIUTSTID-NUM                         
070200     MOVE WS-TIUTSTID-NUM(2:6) TO WS-TIUTSTID                             
070300     MOVE WS-UTSKRIFTSDATUM    TO MOD-UTSKRIFTSDATUM                      
070400                                                                          
070500     MOVE VORD-TIPACKN-SK    TO MOD-TIPACKN-SK                            
070600*    MOVE VORD-KDORDLOT      TO WS-KDORDLOT                               
070700     MOVE VORD-IDLOTNR       TO WS-IDLOTNR                                
070800*    MOVE WS-VAGNNR          TO MOD-VAGNNR                                
070900                                                                          
071000     MOVE VORD-VLORDNTO      TO WS-VLORDNTO                               
071100     MOVE VORD-VKORDNTO      TO WS-VKORDNTO                               
071200     IF US-MEASUREMENT                                                    
071300       COMPUTE WS-VLORDNTO ROUNDED =                                      
071400               WS-VLORDNTO * CONV-M3-TO-FT3 END-COMPUTE                   
071500       COMPUTE WS-VKORDNTO ROUNDED =                                      
071600               WS-VKORDNTO * CONV-KG-TO-LB  END-COMPUTE                   
071700     END-IF                                                               
071800     MOVE WS-VLORDNTO        TO MOD-VLORDNTO                              
071900     MOVE WS-VKORDNTO        TO MOD-VKORDNTO                              
072000                                                                          
072100     MOVE VORD-KVORDRAD      TO MOD-KVORDRAD                              
072200     MOVE VORD-IDDC          TO FRAKTTEXT-INDX                            
072300     MOVE VORD-BEGMRK        TO WS-BEGMRK                                 
072400     MOVE WS-BEGMRK-1        TO MOD-BEGMRK-1                              
072500     MOVE WS-BEGMRK-2        TO MOD-BEGMRK-2                              
072600     .                                                                    
072700     SKIP2                                                                
072800 S02-DATA-FRAAN-WDE401 SECTION.                                           
072900                                                                          
073000     MOVE KORD-IDKUNDRF      TO MOD-IDKUNDRF                              
073100     MOVE KORD-TIORDREG      TO MOD-TIORDREG                              
073200     IF KORD-FLLSBOK = NEJ                                                
073300       IF ENGLISH-TEXT                                                    
073400         MOVE '   STOCKUPD.' TO MOD-LAGERAVBOK                            
073500       ELSE                                                               
073600         MOVE 'LAGERAVBOKN.' TO MOD-LAGERAVBOK                            
073700       END-IF                                                             
073800       MOVE NEJ              TO MOD-FLLSBOK                               
073900     ELSE                                                                 
074000       MOVE MFS-RENSA-FAELT  TO MOD-LAGERAVBOK                            
074100                                MOD-FLLSBOK                               
074200     END-IF                                                               
074300     .                                                                    
074400     SKIP2                                                                
074500 S04-GET-CUSTOMER-INFO           SECTION.                                 
074600                                                                          
074700     PERFORM IMS-GU-GMTA-WDB201                                           
074800                                                                          
074900     IF SEGMENT-FINNS                                                     
075000        IF  GMT-ADGMT = SPACE                                             
075100        AND GMT-BEGMT = SPACE                                             
075200                                                                          
075210           MOVE WS-IDDC TO W-IDDC-B6                                      
075220           PERFORM IMS-GU-WDB601                                          
075221           MOVE DCS-IDFTG         TO W-IDFTG                              
075230                                                                          
075300           MOVE GMT-IDPARTNR      TO W-IDPARTNR                           
075400                                                                          
075500           PERFORM IMS-GU-BETC-WDB101                                     
075600           IF SEGMENT-FINNS                                               
075700                                                                          
075800             MOVE BET-BEBETRAD-1  TO MOD-BEGMT-RAD1                       
075900             MOVE BET-BEBETRAD-2  TO MOD-BEGMT-RAD2                       
076000             MOVE BET-ADBETRAD-1  TO MOD-ADGMT-GATA                       
076100             MOVE BET-ADBETRAD-2  TO MOD-ADGMT-PADR                       
076200           ELSE                                                           
076300             MOVE 'TEXT SAKNAS'   TO MOD-BEGMT-RAD1                       
076400           END-IF                                                         
076500        ELSE                                                              
076600           IF GMT-BEGMT = SPACE                                           
076700              MOVE GMT-ADGMT-GATA TO MOD-BEGMT-RAD1                       
076800              MOVE GMT-ADGMT-PADR TO MOD-BEGMT-RAD2                       
076900              MOVE GMT-ADGMT-LAND TO MOD-ADGMT-GATA                       
077000           ELSE                                                           
077100              MOVE GMT-BEGMT-RAD1 TO MOD-BEGMT-RAD1                       
077200              MOVE GMT-BEGMT-RAD2 TO MOD-BEGMT-RAD2                       
077300              MOVE GMT-ADGMT-GATA TO MOD-ADGMT-GATA                       
077400              MOVE GMT-ADGMT-PADR TO MOD-ADGMT-PADR                       
077500              MOVE GMT-ADGMT-LAND TO MOD-ADGMT-LAND                       
077600           END-IF                                                         
077700        END-IF                                                            
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 S05-BEHANDLA-HAENDELSEBASER SECTION.                                     
078200     SKIP2                                                                
078300     MOVE WS-KDFRAKT         TO W-473B-KDFRAKT                            
078400                                                                          
078500     PERFORM IMS-GU-473B-4732-KVAL                                        
078600                                                                          
078700     IF SEGMENT-FINNS                                                     
078800       PERFORM S06-DATA-FRAAN-RDG-4732                                    
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 S06-DATA-FRAAN-RDG-4732 SECTION.                                         
079300                                                                          
079400     IF WS-IDDC NOT = W-IDDC-B6                                           
079500        MOVE WS-IDDC TO W-IDDC-B6                                         
079600        PERFORM IMS-GU-WDB601                                             
079700     END-IF                                                               
079800     EVALUATE TRUE                                                        
079900     WHEN DCS-CDC OR DCS-SDC AND DCS-IDLANDX2 = 'SE'                      
080000         IF FRAKT-BEFRAKT (1) = SPACE                                     
080100           MOVE FRAKT-BEFRAKT (1) TO MOD-BEFRAKT                          
080200         ELSE                                                             
080300           MOVE FRAKT-BEFRAKT (2) TO MOD-BEFRAKT                          
080400         END-IF                                                           
080500     WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                                 
080600         IF FRAKT-BEFRAKT (4) = SPACE                                     
080700           MOVE FRAKT-BEFRAKT (4) TO MOD-BEFRAKT                          
080800         ELSE                                                             
080900           MOVE FRAKT-BEFRAKT (2) TO MOD-BEFRAKT                          
081000         END-IF                                                           
081100     WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                                 
081200         IF FRAKT-BEFRAKT (6) = SPACE                                     
081300           MOVE FRAKT-BEFRAKT (6) TO MOD-BEFRAKT                          
081400         ELSE                                                             
081500           MOVE FRAKT-BEFRAKT (2) TO MOD-BEFRAKT                          
081600         END-IF                                                           
081700     WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                                 
081800         IF FRAKT-BEFRAKT (5) = SPACE                                     
081900           MOVE FRAKT-BEFRAKT (5) TO MOD-BEFRAKT                          
082000         ELSE                                                             
082100           MOVE FRAKT-BEFRAKT (2) TO MOD-BEFRAKT                          
082200         END-IF                                                           
082300     WHEN OTHER                                                           
082400         MOVE FRAKT-BEFRAKT (2) TO MOD-BEFRAKT                            
082500     END-EVALUATE                                                         
082600     .                                                                    
082700     EJECT                                                                
082800 S07-GET-WDQ301-INFO   SECTION.                                           
082900                                                                          
083000     MOVE WS-IDORDER      TO W-Q301-IDORDER                               
083100     MOVE WS-IDDC         TO W-Q301-IDDC                                  
083200     MOVE WS-IDPRODNR     TO W-Q301-IDPRODNR                              
083300     MOVE WS-IDPLKLST     TO W-Q301-IDPLKLST                              
083400                                                                          
083500     PERFORM IMS-GU-WDQ301-ORQA                                           
083600                                                                          
083700     IF SEGMENT-FINNS                                                     
083800       MOVE ODEL-IDPRC    TO WS-IDPRC                                     
083900     END-IF                                                               
084000     MOVE WS-VAGNNR       TO MOD-VAGNNR                                   
084100     .                                                                    
084200     SKIP2                                                                
084300 S08-HAMTA-IDUSER-FRAN-Q201 SECTION.                                      
084400                                                                          
084500     MOVE WS-IDDISTR         TO DIST19-IDDISTR                            
084600                                TEST-IDDISTR                              
084700     MOVE OHUV-IDKUNDNR      TO TEST-IDKUNDNR                             
084800     MOVE OHUV-IDUSER        TO MOD-IDUSER                                
084900     MOVE OHUV-BELAGINS-DEL1 TO MOD-BELAGINS-DEL1                         
085000     MOVE OHUV-BELAGINS-DEL2 TO MOD-BELAGINS-DEL2                         
085100     MOVE OHUV-BEVARREF      TO MOD-BEVARREF                              
085200                                                                          
085300     PERFORM IMS-GU-GMTA-WDB201                                           
085400                                                                          
085500     IF GMT-FLLDCKND = YES                                                
085600       MOVE OHUV-BEGMT-RAD1      TO MOD-BEGMT-RAD1                        
085700       MOVE OHUV-BEGMT-RAD2      TO MOD-BEGMT-RAD2                        
085800       MOVE OHUV-ADGMT-GATA      TO MOD-ADGMT-GATA                        
085900       MOVE OHUV-ADGMT-PADR      TO MOD-ADGMT-PADR                        
086000       MOVE OHUV-ADGMT-LAND      TO MOD-ADGMT-LAND                        
086100     ELSE                                                                 
086200       IF OHUV-BEGMT = SPACE AND                                          
086300          OHUV-ADGMT = SPACE                                              
086400         PERFORM S04-GET-CUSTOMER-INFO                                    
086500       ELSE                                                               
086600         IF OHUV-BEGMT-RAD1 = SPACE                                       
086700           IF DIST19-SATS                                                 
086800             PERFORM S09-VISA-SATSORDER-INFO                              
086900           ELSE                                                           
087000             MOVE OHUV-ADGMT-GATA TO MOD-ADGMT-GATA                       
087100             MOVE OHUV-ADGMT-PADR TO MOD-ADGMT-PADR                       
087200             MOVE OHUV-ADGMT-PADR TO MOD-ADGMT-PADR                       
087300           END-IF                                                         
087400         ELSE                                                             
087500           MOVE OHUV-BEGMT-RAD1 TO MOD-BEGMT-RAD1                         
087600           MOVE OHUV-BEGMT-RAD2 TO MOD-BEGMT-RAD2                         
087700           IF DIST19-SATS                                                 
087800             PERFORM S09-VISA-SATSORDER-INFO                              
087900           ELSE                                                           
088000             MOVE OHUV-ADGMT-GATA TO MOD-ADGMT-GATA                       
088100             MOVE OHUV-ADGMT-PADR TO MOD-ADGMT-PADR                       
088200             MOVE OHUV-ADGMT-LAND TO MOD-ADGMT-LAND                       
088300           END-IF                                                         
088400         END-IF                                                           
088500       END-IF                                                             
088600     END-IF                                                               
088700     .                                                                    
088800     SKIP2                                                                
088900 S09-VISA-SATSORDER-INFO    SECTION.                                      
089000                                                                          
089100     MOVE KORD-IDARTNR-SATS         TO WS-KORD-IDARTNR-SATS               
089200     MOVE WS-ADGMT-GATA-SATS        TO MOD-ADGMT-GATA                     
089300                                                                          
089400     MOVE KORD-KVBEART-SATS         TO WS-KORD-KVBEART-SATS               
089500     MOVE KORD-KVORDRAD             TO WS-KORD-KVORDRAD                   
089600     MOVE WS-ANT-RAD-ART-SATS       TO MOD-ADGMT-PADR                     
089700     .                                                                    
089800     EJECT                                                                
089900* IMS SEKTIONER                                                           
090000     SKIP3                                                                
090100 IMS-GET-MSG SECTION.                                                     
090200     MOVE '  QC' TO GODK-STATUSKODER                                      
090300     CALL CBLTDLI USING GU                                                
090400                          MSG-PCB                                         
090500                          MSG-IO-AREA                                     
090600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     SKIP3                                                                
090900     .                                                                    
091000 IMS-INSERT-MSG SECTION.                                                  
091100                                                                          
091200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
091300       MOVE '0' TO MFS-KDHUVOMR                                           
091400     END-IF                                                               
091500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
091600     MOVE SPACE TO GODK-STATUSKODER                                       
091700     CALL CBLTDLI USING ISRT                                              
091800                          MSG-PCB                                         
091900                          MSG-IO-AREA                                     
092000                          MFS-IDMOD                                       
092100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092200     PERFORM IMS-STATUSKONTROLL                                           
092300     .                                                                    
092400     EJECT                                                                
092500 IMS-GU-WDE601-KVAL SECTION.                                              
092600     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
092700            DELIMITED BY SIZE INTO SSA1                                   
092800     MOVE '  GE' TO GODK-STATUSKODER                                      
092900     CALL CBLTDLI USING GU                                                
093000                          WDE62-PCB                                       
093100                          DLI-IO-AREA                                     
093200                          SSA1                                            
093300     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     SKIP3                                                                
093600     .                                                                    
093700 IMS-GU-WDE401-ESEQ SECTION.                                              
093800     STRING 'WDE401  (WDE4ESEQ =' W-WDE4E1KY-X ')'                        
093900            DELIMITED BY SIZE INTO SSA1                                   
094000     MOVE '  ' TO GODK-STATUSKODER                                        
094100     CALL CBLTDLI USING GU                                                
094200                          WDE4E-PCB                                       
094300                          DLI-IO-WDE401                                   
094400                          SSA1                                            
094500     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     SKIP3                                                                
094800     .                                                                    
094900 IMS-GU-BETC-WDB101      SECTION.                                         
095000     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
095100            DELIMITED BY SIZE INTO SSA1                                   
095200     MOVE '  GE' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA SSA1                      
095400     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     SKIP3                                                                
095700     .                                                                    
095800 IMS-GU-GMTA-WDB201      SECTION.                                         
095900     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
096000            DELIMITED BY SIZE INTO SSA1                                   
096100     MOVE '  GE' TO GODK-STATUSKODER                                      
096200     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB2 SSA1                 
096300     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     SKIP3                                                                
096600     .                                                                    
096700 IMS-GU-473B-4732-KVAL SECTION.                                           
096800     STRING 'WL473201(WDGXKEY  =' W-473B-WDGXKEY-X ')'                    
096900            DELIMITED BY SIZE INTO SSA1                                   
097000     STRING 'WL473211(KDSEGKEY =' W-4732-KDSEGKEY-X ')'                   
097100            DELIMITED BY SIZE INTO SSA2                                   
097200     MOVE '  GE' TO GODK-STATUSKODER                                      
097300     CALL CBLTDLI USING GU                                                
097400                          4732-PCB                                        
097500                          DLI-IO-AREA                                     
097600                          SSA1                                            
097700                          SSA2                                            
097800     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     EJECT                                                                
098200 IMS-GU-WDE401-KVAL SECTION.                                              
098300     STRING 'WDE401  (WDE401KY =' W-E401-WDE4KEY-X ')'                    
098400            DELIMITED BY SIZE INTO SSA1                                   
098500     MOVE '    ' TO GODK-STATUSKODER                                      
098600     CALL CBLTDLI USING GU                                                
098700                          WDE4-PCB                                        
098800                          DLI-IO-AREA                                     
098900                          SSA1                                            
099000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     SKIP3                                                                
099300     .                                                                    
099400 IMS-GU-WDE601-OKVAL SECTION.                                             
099500     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
099600            DELIMITED BY SIZE INTO SSA1                                   
099700     MOVE '  GE' TO GODK-STATUSKODER                                      
099800     CALL CBLTDLI USING GU                                                
099900                          WDE6-PCB                                        
100000                          DLI-IO-AREA                                     
100100                          SSA1                                            
100200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GU-WDE401-SEK  SECTION.                                              
100700     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
100800            DELIMITED BY SIZE INTO SSA1                                   
100900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
101000     CALL CBLTDLI USING GU                                                
101100                          WDE42-PCB                                       
101200                          DLI-IO-AREA                                     
101300                          SSA1                                            
101400     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
101500                               STATUS-WDE401-SEK-WS                       
101600     PERFORM IMS-STATUSKONTROLL                                           
101700     SKIP3                                                                
101800     .                                                                    
101900 IMS-GN-WDE401-SEK  SECTION.                                              
102000     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
102100            DELIMITED BY SIZE INTO SSA1                                   
102200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
102300     CALL CBLTDLI USING GN                                                
102400                          WDE42-PCB                                       
102500                          DLI-IO-AREA                                     
102600                          SSA1                                            
102700     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
102800                               STATUS-WDE401-SEK-WS                       
102900     PERFORM IMS-STATUSKONTROLL                                           
103000     .                                                                    
103100     EJECT                                                                
103200 IMS-GET-ORQI01-CSEQ SECTION.                                             
103300                                                                          
103400     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
103500             DELIMITED BY SIZE INTO    SSA1                               
103600     MOVE    '  GE'              TO    GODK-STATUSKODER                   
103700     CALL    CBLTDLI             USING GU                                 
103800                                         ORQICSQ-PCB                      
103900                                         DLI-IO-AREA                      
104000                                         SSA1                             
104100     MOVE    ORQICSQ-STATUS-CODE TO    STATUS-WS                          
104200     PERFORM IMS-STATUSKONTROLL                                           
104300     .                                                                    
104400     EJECT                                                                
104500 IMS-GU-WDQ301-ORQA     SECTION.                                          
104600                                                                          
104700     STRING  'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                       
104800             DELIMITED BY SIZE INTO    SSA1                               
104900     MOVE    '  GE'              TO    GODK-STATUSKODER                   
105000     CALL CBLTDLI USING GU ORQA-PCB ODEL-WDQ301 SSA1                      
105100     MOVE    ORQA-STATUS-CODE TO STATUS-WS                                
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     EJECT                                                                
105500 IMS-GU-WDB601    SECTION.                                                
105600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
105700          DELIMITED BY SIZE INTO SSA1                                     
105800     MOVE '  GE' TO GODK-STATUSKODER                                      
105900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
106000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     IF SEGMENT-SAKNAS                                                    
106300        MOVE SPACE TO DCS-KDDC                                            
106400                      DCS-BEGMT-RAD1                                      
106500                      DCS-BEGMT-RAD2                                      
106600                      DCS-ADGMT-GATA                                      
106700                      DCS-ADGMT-PADR                                      
106800                      DCS-ADGMT-LAND                                      
106900     END-IF                                                               
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300 IMS-STATUSKONTROLL SECTION.                                              
107400     SET STATUS-IX TO 1                                                   
107500     SEARCH GODK-STATUS AT END CALL FELLOG                                
107600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
107700     END-SEARCH                                                           
107800     CONTINUE                                                             
107900     .                                                                    
