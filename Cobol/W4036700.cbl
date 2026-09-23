000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036700.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   90/04/20.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        MPP-PROGRAM SOM INGÅR I WOPS-SYSTEMET I PD90-ORDERSYS.           
001100*                                                                         
001200*        PROGRAMMET VISAR VILKA TABELLER SOM SKALL ANVÄNDAS               
001300*        VID ORDER-ENTRY.                                                 
002000*                                                                         
002200*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
002210*                              WLGMTB (WDB3)                              
002220*                              WDB6                                       
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T367                                              
002700*        MID:         W4I36701                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O36701                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003602*    -- CHECKED BY WY2000                                                 
003603*    -- CHECKED BY WY2000                                                 
003610     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W4036700'.            
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  SPAR-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  MAX-TABRADER                PIC S9(9)   VALUE +36  COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004800                                                                          
005000                                                                          
005100 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +1000 COMP SYNC.         
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005900       EJECT                                                              
006000*                                                                         
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-TRANS                          VALUE '4367'.                
007900     88  GODK-TRANS                          VALUE '4367'.                
008000     EJECT                                                                
008100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008200 01  GENERELLA-SUBPROGRAM.                                                
008300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*   -COPY WMEDAREA                                                        
009000     EJECT                                                                
009100*                   ****    PARAMETRAR TILL W005INIT                      
009200*01  -COPY WMSGINIT                                                       
009300     EJECT                                                                
009400 01  FELM-CODES.                                                          
009500     03  FILLER                    PIC X(16)   VALUE 'FELM AREA'.         
009600     03  FELM-KOR-UPPLYSTA-FAELT   PIC X(3)    VALUE '001'.               
009700     03  FELM-URVAL-SAKNAS         PIC X(3)    VALUE '005'.               
009800     03  FELM-PF11-O-TOM-INDATRAD  PIC X(3)    VALUE '011'.               
009900     03  FELM-KUNDUPPG-EJ-KOMPL    PIC X(3)    VALUE '063'.               
010000     03  FELM-FEL-NYCKEL           PIC X(3)    VALUE '401'.               
010100     03  FELM-DISTR-KUND-SAKNAS    PIC X(3)    VALUE '412'.               
010200     03  FELM-FEL-VALKOD           PIC X(3)    VALUE '416'.               
010300     SKIP3                                                                
010400 01  MESSAGE-CODES.                                                       
010500     03  FILLER                    PIC X(16)   VALUE 'INFO AREA'.         
010600     03  INFO-TRYCK-PF11           PIC X(3)    VALUE '003'.               
010700     03  INFO-FOERSTA-SIDAN        PIC X(3)    VALUE '006'.               
010800     03  INFO-UPPDAT-GJORD         PIC X(3)    VALUE '101'.               
010900     03  INFO-MER-INFO-FINNS-PF8   PIC X(3)    VALUE '105'.               
011000     03  INFO-SISTA-SIDAN          PIC X(3)    VALUE '106'.               
011100     EJECT                                                                
011200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011500     SKIP3                                                                
011600*01  MID -COPY W4I36701                                                   
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011900     SKIP3                                                                
012000*01  -COPY WMSGAREA                                                       
012100     EJECT                                                                
012200*    03  MOD -COPY W4O36701   -RED MSG-AREA.                              
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012500     SKIP3                                                                
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100                                                                          
013200 01  NYCKLAR-TILL-DLI.                                                    
013300     03  W-IDDISTR-X.                                                     
013400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013500                                                                          
013600     03  W-IDKUNDNR-X.                                                    
013700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
013800                                                                          
013900     03  W-IDDC-X.                                                        
014000         05  W-IDDC              PIC X(2)    VALUE '00'.                  
014100                                                                          
014400*                                                                         
014500     03  W-IDGMT-X.                                                       
014600         05 W-B201-IDDISTR      PIC S9(5)   COMP-3.                       
014700         05 W-B201-IDKUNDNR     PIC S9(7)   COMP-3.                       
014800*                                                                         
014810     03  W-IDGMT-MAX-X.                                                   
014820         05 W-B201-MAX-IDDISTR  PIC S9(5)   COMP-3.                       
014830         05 W-B201-MAX-IDKUNDNR PIC S9(7)   COMP-3 VALUE +9999999.        
014840*                                                                         
014900     03 W-WDB301KY-X.                                                     
015000         05 W-B301-IDDC             PIC X(2).                             
015100         05 W-B301-IDDISTR          PIC S9(5)   COMP-3.                   
015200         05 W-B301-IDKUNDNR         PIC S9(7)   COMP-3.                   
015300*                                                                         
015400     03 W-WDB301KY-DEF-X.                                                 
015500         05 W-B301-IDDC-DEF         PIC X(2).                             
015600         05 W-B301-IDDISTR-DEF      PIC S9(5)   COMP-3.                   
015700         05 W-B301-IDKUNDNR-DEF     PIC S9(7)                             
015800                                    VALUE +9999999 COMP-3.                
015900     03  W-IDDC-B6-X.                                                     
016000         05 W-IDDC-B6                  PIC X(2).                          
016100                                                                          
018400*    --- STATUS-KOD FRÅN IMS                                              
018500 01  STATUS-WS                   PIC XX.                                  
018600     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018810     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018900                                                                          
019000 01  GODK-STATUSKODER.                                                    
019100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200     SKIP2                                                                
019300 01  SSA1                        PIC X(64).                               
019400 01  SSA2                        PIC X(64).                               
019500     EJECT                                                                
019600*    --- IMS FUNKTIONSKODER                                               
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900*    ---  DLI INPUT-OUTPUT AREA                                           
021310 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
021320*01  -COPY WDB201                                                         
021330     EJECT                                                                
021340 01  FILLER                      PIC X(16)  VALUE 'WDB3-AREA'.            
021350*01  -COPY WDB301                                                         
021351                                                                          
021352 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021353 01   DLI-IO-AREA-B601.                                                   
021354*     03  -COPY WDB601                                                    
021355                                                                          
021360     EJECT                                                                
021400 LINKAGE SECTION.                                                         
021500                                                                          
021600*01  -COPY W0009      -PRE MSG-                                           
021700     EJECT                                                                
021800*01  -COPY W0008     -PRE USEA-                                           
021900     05  FILLER              PIC X.                                       
022000     EJECT                                                                
022100*01  -COPY W0008      -PRE GMTA-                                          
022200     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE GMTB-                                          
023000     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023410*01  -COPY W0008      -PRE WDB6-                                          
023420     05  FILLER                  PIC X.                                   
023430     EJECT                                                                
023500 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
023600                                    GMTA-PCB                              
023700                                    GMTB-PCB                              
023710                                    WDB6-PCB.                             
023800     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
023900                                    GMTA-PCB                              
024000                                    GMTB-PCB                              
024010                                    WDB6-PCB.                             
024100                                                                          
024200     PERFORM IMS-GET-MSG                                                  
024300     IF SEGMENT-FINNS                                                     
024400       PERFORM A-INIT                                                     
024500       PERFORM B-KOLLA-NYCKLAR                                            
024600       IF NYCKLAR-OK                                                      
025600         IF MFS-FIRST                                                     
025700           PERFORM D-FOERSTA-SIDAN                                        
025800         ELSE                                                             
025900           IF MFS-NEXT                                                    
026000             PERFORM E-NAESTA-SIDAN                                       
026100           ELSE                                                           
026200             PERFORM F-SAMMA-SIDA                                         
026300           END-IF                                                         
026400         END-IF                                                           
026600         PERFORM H-LAES-VISA-TABELL                                       
027100       END-IF                                                             
027200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027300       PERFORM IMS-INSERT-MSG                                             
027400     END-IF                                                               
027500                                                                          
027600     MOVE ZERO TO RETURN-CODE                                             
027700     GOBACK                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 A-INIT SECTION.                                                          
028100                                                                          
028200     IF MSG-DUBBLA-TRANSKODER                                             
028300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36701                 
028400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028600     ELSE                                                                 
028700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36701                  
028800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029000     END-IF                                                               
029100                                                                          
029200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
029300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029500                                                                          
029600     MOVE LOW-VALUE TO MSG-AREA                                           
029700     MOVE 'W4O367N1' TO MFS-IDMOD                                         
029800     MOVE '4367' TO MOD-IDTRANS                                           
029900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
030000                             MOD-TEMFSINF                                 
030100     IF NOT EGEN-TRANS                                                    
030200       MOVE SPACE TO MFS-KDTRTYP                                          
030300       MOVE '7' TO MFS-IDPFK                                              
030400     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 B-KOLLA-NYCKLAR SECTION.                                                 
031400                                                                          
031500     IF GODK-TRANS                                                        
031600                                                                          
031700       MOVE ALL '+'           TO MSGI-WMSGINIT                            
031800       MOVE '001'             TO MSGI-KDCALL                              
031900       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
032000       MOVE '4367'            TO MSGI-IDTRANS                             
032100       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
032200       IF MFS-IDTRANS = '4367'                                            
032300          MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                            
032400          MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                           
032500       END-IF                                                             
032600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
032610                                                                          
032620       IF MSGI-IDLAND-SPR = 'GB'                                          
032630         MOVE 'GB ' TO MED-IDSKYLT                                        
032640       ELSE                                                               
032650         MOVE 'S  ' TO MED-IDSKYLT                                        
032660       END-IF                                                             
032700                                                                          
032800       MOVE JA TO NYCKLAR-SW                                              
032810                                                                          
032900       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                             
033000                               MOD-IDKUNDNR-IN                            
033010                               MOD-IDDC-IN                                
033100                                                                          
033600       IF MID-IDDISTR-IN = ALL '+'                                        
033700         MOVE MID-IDDISTR-UT TO WS-IDDISTR                                
033800         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
033900                                                                          
034000         IF MID-IDKUNDNR-IN = ALL '+'                                     
034100           IF MID-IDKUNDNR-UT = SPACE                                     
034200             MOVE SPACE TO WS-IDKUNDNR                                    
034300             INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO          
034500           ELSE                                                           
034600             MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                          
034700             INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO          
034900           END-IF                                                         
035000         ELSE                                                             
035100           MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                            
035110           INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO            
035200           MOVE '7' TO MFS-IDPFK                                          
035300           MOVE SPACE TO MFS-KDTRTYP                                      
035500         END-IF                                                           
035600                                                                          
035700       ELSE                                                               
035800         MOVE MID-IDDISTR-IN TO WS-IDDISTR                                
035810         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
035900         MOVE '7'         TO MFS-IDPFK                                    
036000         MOVE SPACE       TO MFS-KDTRTYP                                  
036100                                                                          
036200         IF MID-IDKUNDNR-IN NOT = ALL '+'                                 
036300           MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                            
036310           INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO            
036500         ELSE                                                             
036600           MOVE SPACE TO WS-IDKUNDNR                                      
036700           INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO            
036900         END-IF                                                           
037000                                                                          
037100       END-IF                                                             
037200                                                                          
037300       IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                        
037400         MOVE WS-IDDISTR TO W-B201-IDDISTR                                
037410                            W-B201-MAX-IDDISTR                            
037500                            W-B301-IDDISTR                                
037600                            W-B301-IDDISTR-DEF                            
037700       ELSE                                                               
037800         MOVE NEJ TO NYCKLAR-SW                                           
037900       END-IF                                                             
038000                                                                          
038100                                                                          
038200       IF WS-IDKUNDNR NUMERIC                                             
038300         MOVE WS-IDKUNDNR TO W-B201-IDKUNDNR                              
038600       ELSE                                                               
038700         MOVE NEJ TO NYCKLAR-SW                                           
038800       END-IF                                                             
038900                                                                          
039000       MOVE MSGI-IDDC     TO W-IDDC-B6                                    
039010       PERFORM IMS-GU-WDB601                                              
039100                                                                          
039200       IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                       
039201         MOVE NEJ                     TO NYCKLAR-SW                       
039202       ELSE                                                               
039210         MOVE DCS-IDDC    TO W-B301-IDDC                                  
039220                             W-B301-IDDC-DEF                              
039600       END-IF                                                             
039700                                                                          
040800       IF NYCKLAR-OK                                                      
040900         MOVE WS-IDDISTR  TO MOD-IDDISTR-UT                               
041000         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
041100                                                                          
041200         MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                              
041300         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
041400                                                                          
041500         MOVE DCS-IDDC    TO MOD-IDDC-UT                                  
041600         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
041700       ELSE                                                               
041800         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT MOD-IDKUNDNR-UT           
041900                                 MOD-IDDC-UT                              
042000       END-IF                                                             
042100                                                                          
044100       IF NYCKLAR-FEL                                                     
044200         MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                             
044300         CALL WMEDKONV USING MED-WMEDAREA                                 
044400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
044600         PERFORM MFS-RENSA-FAELT-UT                                       
044700       END-IF                                                             
044800                                                                          
045200     ELSE                                                                 
045300       MOVE NEJ TO NYCKLAR-SW                                             
045400       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                             
045500                               MOD-IDKUNDNR-IN                            
045700       PERFORM MFS-RENSA-FAELT-UT                                         
045800     END-IF                                                               
045900     .                                                                    
047000     EJECT                                                                
047100 D-FOERSTA-SIDAN SECTION.                                                 
047200                                                                          
047400     MOVE WS-IDKUNDNR   TO W-B201-IDKUNDNR                                
047700     MOVE INFO-FOERSTA-SIDAN TO MED-IDMFSFEL                              
047800     CALL WMEDKONV USING MED-WMEDAREA                                     
047900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
048300     .                                                                    
048400     EJECT                                                                
048500 E-NAESTA-SIDAN SECTION.                                                  
048600                                                                          
048800     MOVE MID-IDKUNDNR-NEXT TO W-B201-IDKUNDNR                            
049400       .                                                                  
049500     EJECT                                                                
049600 F-SAMMA-SIDA SECTION.                                                    
049700                                                                          
049900     MOVE MID-IDKUNDNR-ENTER TO W-B201-IDKUNDNR                           
051600     .                                                                    
077000     EJECT                                                                
077100 H-LAES-VISA-TABELL SECTION.                                              
077200                                                                          
077300     MOVE +1 TO RAD-INDX                                                  
077400     PERFORM IMS-GU-GMTA01-WDB201                                         
077600                                                                          
077610     PERFORM UNTIL SEGMENT-SLUT                                           
077611             OR    SEGMENT-SAKNAS                                         
077620             OR GMT-TISTADAT > 0                                          
077621         PERFORM IMS-GN-GMTA01-WDB201                                     
077630     END-PERFORM                                                          
077640                                                                          
077700     IF SEGMENT-FINNS                                                     
077800       MOVE GMT-IDKUNDNR  TO MOD-IDKUNDNR-ENTER                           
078100     END-IF                                                               
078200                                                                          
078300     PERFORM HA-VISA-TABELL-RADER                                         
078400                                                                          
078500     PERFORM HB-KOLLA-OM-FLER-SIDOR                                       
078600                                                                          
079500     .                                                                    
079600     EJECT                                                                
079700 HA-VISA-TABELL-RADER SECTION.                                            
079800                                                                          
079900     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
080000       IF SEGMENT-FINNS                                                   
080100         MOVE GMT-IDKUNDNR      TO W-B301-IDKUNDNR                        
080101                                   MOD-IDKUNDNR(RAD-INDX)                 
080110                                                                          
080210         PERFORM IMS-GU-GMTB01-WDB301                                     
080300         IF SEGMENT-FINNS                                                 
080500           MOVE DC-IDGMTOMR     TO MOD-IDGMTOMR(RAD-INDX)                 
080600           MOVE DC-IDPRCTAB     TO MOD-IDPRCTAB(RAD-INDX)                 
080700           MOVE DC-IDPKLTAB     TO MOD-IDPKLTAB(RAD-INDX)                 
080800           INSPECT MOD-IDPKLTAB(RAD-INDX) REPLACING                       
080900                                   LEADING ZERO BY SPACE                  
081000           IF MOD-IDPKLTAB(RAD-INDX) = SPACE                              
081100             MOVE ' 0'          TO MOD-IDPKLTAB(RAD-INDX)                 
081200           END-IF                                                         
081300           ADD +1 TO RAD-INDX                                             
081400         ELSE                                                             
081600           MOVE MFS-RENSA-FAELT TO MOD-IDGMTOMR(RAD-INDX)                 
081700                                   MOD-IDPRCTAB(RAD-INDX)                 
081800                                   MOD-IDPKLTAB(RAD-INDX)                 
081810           ADD +1 TO RAD-INDX                                             
081900         END-IF                                                           
081910         PERFORM IMS-GN-GMTA01-WDB201                                     
082000         PERFORM UNTIL SEGMENT-SLUT                                       
082100                 OR    SEGMENT-SAKNAS                                     
082200                 OR GMT-TISTADAT > 0                                      
082300             PERFORM IMS-GN-GMTA01-WDB201                                 
082400         END-PERFORM                                                      
082800       ELSE                                                               
082900         MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR(RAD-INDX)                   
083000                                 MOD-IDGMTOMR(RAD-INDX)                   
083100                                 MOD-IDPRCTAB(RAD-INDX)                   
083200                                 MOD-IDPKLTAB(RAD-INDX)                   
083300         ADD +1 TO RAD-INDX                                               
083400       END-IF                                                             
083500     END-PERFORM                                                          
083600     .                                                                    
083700     EJECT                                                                
083800 HB-KOLLA-OM-FLER-SIDOR SECTION.                                          
083900                                                                          
084100       IF SEGMENT-FINNS                                                   
084200         MOVE GMT-IDKUNDNR            TO MOD-IDKUNDNR-NEXT                
084300         MOVE INFO-MER-INFO-FINNS-PF8 TO MED-IDMFSINF                     
084400         CALL WMEDKONV USING MED-WMEDAREA                                 
084500         MOVE MED-MFSINF              TO MOD-TEMFSINF                     
084600       ELSE                                                               
084700         MOVE MOD-IDKUNDNR-ENTER      TO MOD-IDKUNDNR-NEXT                
085000         MOVE INFO-SISTA-SIDAN        TO MED-IDMFSINF                     
085100         CALL WMEDKONV USING MED-WMEDAREA                                 
085200         MOVE MED-MFSINF              TO MOD-TEMFSINF                     
085400       END-IF                                                             
085600     .                                                                    
088000     EJECT                                                                
088100 MFS-RENSA-FAELT-UT SECTION.                                              
088200                                                                          
088300*    --- ALLA UTDATA-FÄLT                                                 
088400     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-ENTER                           
088500                             MOD-IDKUNDNR-NEXT                            
088600     MOVE +1              TO RAD-INDX                                     
088700     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
088800       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR(RAD-INDX)                     
088900                               MOD-IDGMTOMR(RAD-INDX)                     
089000                               MOD-IDPRCTAB(RAD-INDX)                     
089100                               MOD-IDPKLTAB(RAD-INDX)                     
089200       ADD +1 TO RAD-INDX                                                 
089300     END-PERFORM                                                          
089400     .                                                                    
093500     EJECT                                                                
093600* --- IMS SEKTIONER ---                                                   
093700     SKIP3                                                                
093800 IMS-GET-MSG SECTION.                                                     
093900                                                                          
094000     MOVE '  QC' TO GODK-STATUSKODER                                      
094100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     SKIP3                                                                
094600 IMS-INSERT-MSG SECTION.                                                  
094700                                                                          
094710     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
094900       MOVE '0' TO MFS-KDHUVOMR                                           
095000     END-IF                                                               
095100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095200     MOVE SPACE TO GODK-STATUSKODER                                       
095300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     EJECT                                                                
095800 IMS-GU-GMTB01-WDB301 SECTION.                                            
095900                                                                          
096000     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
096100                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
096200          DELIMITED BY SIZE INTO SSA1                                     
096300     MOVE '  GE' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING GU GMTB-PCB DC-WDB301 SSA1                        
096500     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
103010     EJECT                                                                
103011 IMS-GU-GMTA01-WDB201      SECTION.                                       
103012     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X                               
103013                    '*IDGMT    <' W-IDGMT-MAX-X ')'                       
103014            DELIMITED BY SIZE INTO SSA1                                   
103015     MOVE '  GE' TO GODK-STATUSKODER                                      
103016     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
103017     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
103018     PERFORM IMS-STATUSKONTROLL                                           
103020     .                                                                    
103021     SKIP3                                                                
103022 IMS-GN-GMTA01-WDB201      SECTION.                                       
103023     STRING 'WLGMTA01(IDGMT   =>' W-IDGMT-X                               
103024                    '*IDGMT    <' W-IDGMT-MAX-X ')'                       
103030            DELIMITED BY SIZE INTO SSA1                                   
103040     MOVE '  GEGB' TO GODK-STATUSKODER                                    
103050     CALL CBLTDLI USING GN GMTA-PCB GMT-WDB201 SSA1                       
103060     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
103070     PERFORM IMS-STATUSKONTROLL                                           
103090     .                                                                    
103100     SKIP3                                                                
103200 IMS-GU-WDB601    SECTION.                                                
103300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
103400          DELIMITED BY SIZE INTO SSA1                                     
103500     MOVE '  GE' TO GODK-STATUSKODER                                      
103600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
103700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
103800     PERFORM IMS-STATUSKONTROLL                                           
103900     IF SEGMENT-SAKNAS                                                    
104000         MOVE SPACE TO DCS-KDDC                                           
104100     END-IF                                                               
104200     .                                                                    
107900 IMS-STATUSKONTROLL SECTION.                                              
108000                                                                          
108100     SET STATUS-IX TO 1                                                   
108200     SEARCH GODK-STATUS                                                   
108300       AT END CALL FELLOG                                                 
108400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
108500     END-SEARCH                                                           
108600     .                                                                    
