000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6020100.                                                
000400*AUTHOR.         GERRY  CARMICHAEL / ARCHANA BHAT                         
000500*DATE-WRITTEN.   91/10/16 / MAY 2012.                                     
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN FRÅGEBILD SOM VISAR                             
001100*        INFORMATION OM KONTROLLRAPPORTER                                 
001200*        BEROENDE PÅ IFYLLDA NYCKLAR.HOPP                                 
001300*        KAN SKE TILL OLIKA BILDER I SERIEN                               
001400*        OM MAN ANGE KANTKOD OCH VALD PF-                                 
001500*        TANGENT.                                                         
001600*                                                                         
001700*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
001800*        PROGRAMMET LÄSER      W6KVAF (W6H7A1)                            
001900*        PROGRAMMET LÄSER      W6KVAG (W6H7B1)                            
002000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T201                                              
002400*        MID:         W6I20101                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W6O20101                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6020100'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004700                                                                          
004800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004900     88  ALLT-OK                             VALUE 'J'.                   
005000                                                                          
005100 77  HOPP-SW                     PIC X       VALUE 'J'.                   
005200     88  HOPP                                VALUE 'J'.                   
005300                                                                          
005400 77  KDBEHX-SW                   PIC X       VALUE 'J'.                   
005500     88  KDBEHX-FOUND                        VALUE 'J'.                   
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700                                                                          
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '6201'.                
006100     88  GODK-MID                            VALUE '6201' '6202'          
006200                                                   '6203' '6204'          
006300                                                   '6205' '6206'          
006400                                                   '6207' '6208'          
006500                                                   '6209'.                
006600     88  HELP-MID                            VALUE '0551'.                
006700     EJECT                                                                
006800*      --- VALID IDDC CODES                                               
006900*                                                                         
007000*01    -COPY WWDCKONS                                                     
007100       EJECT                                                              
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  W6020110                PIC X(8)    VALUE 'W6020110'.            
007800     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007900     EJECT                                                                
008000*01 -COPY WMSGINIT                                                        
008100     SKIP3                                                                
008200 01  W-IDMSG-ERROR               PIC X(3).                                
008300     88   WRONG-KEY                         VALUE '022'.                  
008400*                                                                         
008500 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
008600 01  REQU-AREA.                                                           
008700*    03 -COPY WZ01REQU                                                    
008800*    03 -COPY W60201I1                                                    
008900 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +13.               
009000     EJECT                                                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009300 01  RESP-AREA.                                                           
009400*    03 -COPY WZ01RESP                                                    
009500*    03 -COPY W60201O1                                                    
009600*                                                                         
009700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010000     SKIP3                                                                
010100*01  MID -COPY W6I20101                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010400     SKIP3                                                                
010500*01  -COPY WMSGAREA                                                       
010600     EJECT                                                                
010700     03  MOD REDEFINES MSG-AREA.                                          
010800*      05  -COPY W6O20101                                                 
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011100     SKIP3                                                                
011200*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
011500     SKIP3                                                                
011600*01  -COPY WL01MCNV                                                       
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'ALT-MSG-AREA'.        
011900 01  ALT-MSG-IO-AREA.                                                     
012000   03 ALT-LL                     PIC S9(4)  VALUE +29  COMP SYNC.         
012100   03 ALT-Z1                     PIC X.                                   
012200   03 ALT-Z2                     PIC X.                                   
012300   03 ALT-TRANSKOD               PIC X(8).                                
012400   03 ALT-IDTRANS                PIC X(4)   VALUE '6201'.                 
012500   03 ALT-SPRAK                  PIC X.                                   
012600   03 ALT-IDKR-IN                PIC 9(5).                                
012700   03 ALT-IDKR-UT                PIC X(5)   VALUE SPACE.                  
012800   03 ALT-IDDC-IN                PIC X(2).                                
012900     EJECT                                                                
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100     SKIP2                                                                
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     88  BASEN-SLUT                          VALUE 'GB'.                  
013800     SKIP2                                                                
013900 01  GODK-STATUSKODER.                                                    
014000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200*    --- IMS FUNKTIONSKODER                                               
014300*01  -COPY W0003                                                          
014400     EJECT                                                                
014500*    ---  DLI INPUT-OUTPUT AREA                                           
014600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014700     SKIP3                                                                
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000                                                                          
015100*01  -COPY W0009   -PRE MSG-                                              
015200     EJECT                                                                
015300*01  -COPY W0009   -PRE ALTA-                                             
015400     EJECT                                                                
015500*01  -COPY W0009   -PRE ALTB-                                             
015600     EJECT                                                                
015700*01  -COPY W0009   -PRE ALTC-                                             
015800     EJECT                                                                
015900*01  -COPY W0009   -PRE ALTD-                                             
016000     EJECT                                                                
016100*01  -COPY W0009   -PRE ALTE-                                             
016200     EJECT                                                                
016300*01  -COPY W0008  -PRE USEA-                                              
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016810*01  -COPY W0008  -PRE KVAE-                                              
016820     05  FILLER                  PIC X.                                   
016830     EJECT                                                                
016900*01  -COPY W0008  -PRE KVAF-                                              
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200*01  -COPY W0008  -PRE KVAG-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE BENA-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE WDP3-                                              
017900     05  WDP3-KEY-FB-AREA-KDARBTYP      PIC X(8).                         
018000     05  WDP3-KEY-FB-AREA-IDPERSON      PIC S9(3) COMP-3.                 
018100     EJECT                                                                
018200*01  -COPY W0008  -PRE WDK6-                                              
018201     05  FILLER                  PIC X.                                   
018210     EJECT                                                                
018220*01  -COPY W0008  -PRE WDK7-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018220*01  -COPY W0008  -PRE WDB6-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500 PROCEDURE DIVISION  USING MSG-PCB ALTA-PCB ALTB-PCB ALTC-PCB             
018600                           ALTD-PCB ALTE-PCB USEA-PCB                     
018700                           KVAE-PCB KVAF-PCB                              
018800                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
018810                           WDK7-PCB WDB6-PCB.                             
018900     ENTRY 'DLITCBL' USING MSG-PCB ALTA-PCB ALTB-PCB ALTC-PCB             
019000                           ALTD-PCB ALTE-PCB USEA-PCB                     
019100                           KVAE-PCB KVAF-PCB                              
019200                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
019210                           WDK7-PCB WDB6-PCB.                             
019300                                                                          
019400     PERFORM IMS-GET-MSG                                                  
019500     IF SEGMENT-FINNS                                                     
019600       PERFORM A-INIT                                                     
019700       PERFORM B-INIT-KEYS                                                
019800       PERFORM G-INIT-REQU                                                
019900       IF MFS-FIRST                                                       
020000         SET REQU-FIRST       TO TRUE                                     
020100         PERFORM C-FOERSTA-SIDA                                           
020200       ELSE                                                               
020300         IF MFS-NEXT                                                      
020400           SET REQU-NEXT      TO TRUE                                     
020500           PERFORM D-NAESTA-SIDA                                          
020600         ELSE                                                             
020700           IF MFS-IDPFK = 'A' OR 'B' OR 'C' OR 'D' OR 'E'                 
020800              SET REQU-UPDATE TO TRUE                                     
020900              PERFORM H-HOPP-TILL-ANNAN-BILD                              
021000           ELSE                                                           
021100              SET REQU-QUERY  TO TRUE                                     
021200              PERFORM E-SAMMA-SIDA                                        
021300           END-IF                                                         
021400         END-IF                                                           
021500       END-IF                                                             
021600       IF ALLT-OK                                                         
021700          PERFORM F-CALL-BIZ-LOGIC-W6020110                               
021800       END-IF                                                             
021900       IF NOT HOPP                                                        
022000          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20101 + 4                   
022100          PERFORM IMS-INSERT-MSG                                          
022200       END-IF                                                             
022300     END-IF                                                               
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000                                                                          
023100     IF MSG-DUBBLA-TRANSKODER                                             
023200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I20101                 
023300       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
023400       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
023500                                             ALT-SPRAK                    
023600     ELSE                                                                 
023700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I20101                 
023800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023900       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024000                                             ALT-SPRAK                    
024100     END-IF                                                               
024200                                                                          
024300     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
024400     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
024500     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
024600                                                                          
024700     MOVE LOW-VALUE       TO MSG-AREA                                     
024800     MOVE 'W6O201N1'      TO MFS-IDMOD                                    
024900     MOVE '6201'          TO MOD-IDTRANS                                  
025000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025100                                                                          
025200     IF EGEN-MID OR HELP-MID                                              
025300       CONTINUE                                                           
025400     ELSE                                                                 
025500       MOVE SPACE         TO MFS-KDTRTYP                                  
025600       MOVE '7'           TO MFS-IDPFK                                    
025700     END-IF                                                               
025800                                                                          
025900     .                                                                    
026000     EJECT                                                                
026100 B-INIT-KEYS SECTION.                                                     
026200                                                                          
026300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026400     MOVE '001'             TO MSGI-KDCALL                                
026500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026700     MOVE '6201'            TO MSGI-IDTRANS                               
026800     IF MFS-IDTRANS = '6201'                                              
026900       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
027000     ELSE                                                                 
027100       MOVE ZERO            TO MID-KDKRSTA-IN                             
027200                               MID-KDBEHX-IN                              
027300                               MID-KDPERSTYP-IN                           
027400                               MID-IDPERSON-IN                            
027500                               MID-IDTYP-IN                               
027600       MOVE SPACE           TO MID-KDPERSTYP-IN                           
027700       MOVE NEJ             TO MID-FLANNULL-IN                            
027800     END-IF                                                               
027900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028000                                                                          
028100     MOVE NEJ               TO HOPP-SW                                    
028200     MOVE MSGI-IDARTNR      TO REQU-IDARTNR-KEY                           
028300     INSPECT REQU-IDARTNR-KEY REPLACING ALL SPACE BY ZERO                 
028400                                                                          
028500     MOVE MFS-RENSA-FAELT   TO MOD-KDKRSTA-IN                             
028600                               MOD-KDBEHX-IN                              
028700                               MOD-KDPERSTYP-IN                           
028800                               MOD-IDPERSON-IN                            
028900                               MOD-IDARTNR-IN                             
029000                               MOD-IDTYP-IN                               
029100                               MOD-FLANNULL-IN                            
029200                               MOD-IDDC-IN                                
029300                                                                          
029400     IF MID-IDDC-IN = ALL '+' OR SPACE                                    
029500       IF MSGI-IDDC   = ALL '+' OR SPACE                                  
029600         CONTINUE                                                         
029700       ELSE                                                               
029800         MOVE MSGI-IDDC       TO REQU-IDDC-KEY                            
029900       END-IF                                                             
030000     ELSE                                                                 
030100       MOVE MID-IDDC-IN       TO REQU-IDDC-KEY                            
030200       MOVE '7'               TO MFS-IDPFK                                
030300       MOVE SPACE             TO MFS-KDTRTYP                              
030400     END-IF                                                               
030500     MOVE REQU-IDDC-KEY       TO MOD-IDDC-UT                              
030600                                                                          
030700     IF MID-FLANNULL-IN = ALL '+' OR SPACE                                
030800       IF MID-FLANNULL-UT = SPACE                                         
030900         MOVE NEJ             TO REQU-FLANNULL-KEY                        
031000       ELSE                                                               
031100         MOVE MID-FLANNULL-UT TO REQU-FLANNULL-KEY                        
031200       END-IF                                                             
031300     ELSE                                                                 
031400       MOVE MID-FLANNULL-IN   TO REQU-FLANNULL-KEY                        
031500       MOVE '7'               TO MFS-IDPFK                                
031600       MOVE SPACE             TO MFS-KDTRTYP                              
031700     END-IF                                                               
031800     IF REQU-FLANNULL-KEY = 'J' OR 'Y' OR 'X'                             
031900        MOVE 'JA'             TO MOD-FLANNULL-UT                          
032000     ELSE                                                                 
032100        MOVE REQU-FLANNULL-KEY TO MOD-FLANNULL-UT                         
032200     END-IF                                                               
032300                                                                          
032400     IF MID-KDKRSTA-IN = ALL '+'                                          
032500       MOVE MID-KDKRSTA-UT    TO REQU-KDKRSTA-KEY                         
032600       INSPECT REQU-KDKRSTA-KEY REPLACING LEADING SPACE BY ZERO           
032700     ELSE                                                                 
032800       MOVE MID-KDKRSTA-IN    TO REQU-KDKRSTA-KEY                         
032900       INSPECT REQU-KDKRSTA-KEY REPLACING LEADING SPACE BY ZERO           
033000       MOVE '7'               TO MFS-IDPFK                                
033100       MOVE SPACE             TO MFS-KDTRTYP                              
033200     END-IF                                                               
033300                                                                          
033400     IF MID-IDARTNR-IN = ALL '+'                                          
033500       CONTINUE                                                           
033600     ELSE                                                                 
033700       MOVE '7'               TO MFS-IDPFK                                
033800       MOVE SPACE             TO MFS-KDTRTYP                              
033900     END-IF                                                               
034000                                                                          
034100     IF MID-KDPERSTYP-IN = ALL '+'                                        
034200       MOVE MID-KDPERSTYP-UT  TO REQU-KDPERSTYP-KEY                       
034300     ELSE                                                                 
034400       MOVE MID-KDPERSTYP-IN  TO REQU-KDPERSTYP-KEY                       
034500       MOVE '7'               TO MFS-IDPFK                                
034600       MOVE SPACE             TO MFS-KDTRTYP                              
034700     END-IF                                                               
034800                                                                          
034900     IF MID-IDPERSON-IN = ALL '+'                                         
035000       MOVE MID-IDPERSON-UT   TO REQU-IDPERSON-KEY                        
035100     ELSE                                                                 
035200       MOVE MID-IDPERSON-IN   TO REQU-IDPERSON-KEY                        
035300       MOVE '7'               TO MFS-IDPFK                                
035400       MOVE SPACE             TO MFS-KDTRTYP                              
035500     END-IF                                                               
035600                                                                          
035700     IF MID-KDBEHX-IN = ALL '+'                                           
035800       MOVE MID-KDBEHX-UT     TO REQU-KDBEHX-KEY                          
035900     ELSE                                                                 
036000       MOVE MID-KDBEHX-IN     TO REQU-KDBEHX-KEY                          
036100       MOVE '7'               TO MFS-IDPFK                                
036200       MOVE SPACE             TO MFS-KDTRTYP                              
036300     END-IF                                                               
036400                                                                          
036500     IF MID-IDTYP-IN = ALL '+'                                            
036600       MOVE MID-IDTYP-UT      TO REQU-IDTYP-KEY                           
036700     ELSE                                                                 
036800       MOVE MID-IDTYP-IN      TO REQU-IDTYP-KEY                           
036900       MOVE '7'               TO MFS-IDPFK                                
037000       MOVE SPACE             TO MFS-KDTRTYP                              
037100     END-IF                                                               
037200                                                                          
037400     MOVE REQU-KDKRSTA-KEY       TO MOD-KDKRSTA-UT                        
037500     MOVE REQU-KDPERSTYP-KEY     TO MOD-KDPERSTYP-UT                      
037600     MOVE REQU-IDPERSON-KEY      TO MOD-IDPERSON-UT                       
037700     MOVE REQU-KDBEHX-KEY        TO MOD-KDBEHX-UT                         
037800     MOVE REQU-IDARTNR-KEY       TO MOD-IDARTNR-UT                        
037900     MOVE REQU-IDTYP-KEY         TO MOD-IDTYP-UT                          
038000     MOVE REQU-IDDC-KEY          TO MOD-IDDC-UT                           
038100     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
038200     INSPECT MOD-KDBEHX-UT   REPLACING LEADING ZERO BY SPACE              
038300     INSPECT MOD-IDPERSON-UT REPLACING LEADING ZERO BY SPACE              
038400     INSPECT MOD-IDTYP-UT    REPLACING LEADING ZERO BY SPACE              
039600     .                                                                    
039700     EJECT                                                                
039800 C-FOERSTA-SIDA SECTION.                                                  
039900                                                                          
040000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
040100     MOVE ZERO  TO MOD-IDDC-ENTER                                         
040200                   MOD-IDDC-NEXT                                          
040300                   MOD-TIREGDAT-ENTER                                     
040400                   MOD-TIREGDAT-NEXT                                      
040500                   MOD-KDKRSTA-ENTER                                      
040600                   MOD-KDKRSTA-NEXT                                       
040700     MOVE SPACE TO MOD-IDLOPNRM-ENTER                                     
040800                   MOD-IDLOPNRM-NEXT                                      
040900                   MOD-IDLEVNR-ENTER                                      
041000                   MOD-IDLEVNR-NEXT                                       
041100     MOVE ZERO  TO MOD-IDKR-ENTER                                         
041200                   MOD-IDKR-NEXT                                          
041300                   MOD-IDARTNR-ENTER                                      
041400                   MOD-IDARTNR-NEXT                                       
041500                   MOD-DAREGDAT-9KOMPL-ENTER                              
041600                   MOD-DAREGDAT-9KOMPL-NEXT                               
041700                   MOD-KVKRKNTR-ENTER                                     
041800                   MOD-KVKRKNTR-NEXT                                      
041900     .                                                                    
042000     EJECT                                                                
042100 D-NAESTA-SIDA SECTION.                                                   
042200                                                                          
042300     MOVE MID-IDDC-NEXT     TO REQU-IDDC-START                            
042400                                                                          
042500     MOVE MID-KDKRSTA-NEXT  TO REQU-KDKRSTA-START                         
042600                                                                          
042700     MOVE MID-IDLOPNRM-NEXT TO REQU-IDLOPNRM-START                        
042800     MOVE MID-TIREGDAT-NEXT TO REQU-TIREGDAT-START                        
042900                                                                          
043000     MOVE MID-IDFTG-NEXT    TO REQU-IDFTG-START                           
043100                                                                          
043200     MOVE MID-IDARTNR-NEXT  TO REQU-IDARTNR-START                         
043300                                                                          
043400     MOVE MID-DAREGDAT-9KOMPL-NEXT                                        
043500                            TO REQU-DAREGDAT-9KOMPL-START                 
043600                                                                          
043700     MOVE MID-IDLEVNR-NEXT  TO REQU-IDLEVNR-START                         
043800     MOVE MID-KVKRKNTR-NEXT TO REQU-KVKRKNTR-START                        
043900                                                                          
044000     MOVE MID-IDKR-NEXT     TO REQU-IDKR-START                            
044100     .                                                                    
044200     EJECT                                                                
044300 E-SAMMA-SIDA SECTION.                                                    
044400                                                                          
044500     IF EGEN-MID OR HELP-MID                                              
044600                                                                          
044700       MOVE MID-IDDC-ENTER     TO REQU-IDDC-START                         
044800       MOVE MID-TIREGDAT-ENTER TO REQU-TIREGDAT-START                     
044900       MOVE MID-KDKRSTA-ENTER  TO REQU-KDKRSTA-START                      
045000       MOVE MID-IDLOPNRM-ENTER TO REQU-IDLOPNRM-START                     
045100       MOVE MID-IDARTNR-ENTER  TO REQU-IDARTNR-START                      
045200       MOVE MID-IDKR-ENTER     TO REQU-IDKR-START                         
045300       MOVE MID-DAREGDAT-9KOMPL-ENTER                                     
045400                               TO REQU-DAREGDAT-9KOMPL-START              
045500       MOVE MID-IDLEVNR-ENTER  TO REQU-IDLEVNR-START                      
045600       MOVE MID-KVKRKNTR-ENTER TO REQU-KVKRKNTR-START                     
045700     ELSE                                                                 
045800       PERFORM MFS-RENSA-FAELT-IN                                         
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 F-CALL-BIZ-LOGIC-W6020110 SECTION.                                       
046300                                                                          
046400     CALL W6020110 USING REQU-AREA RESP-AREA MAX-KVRADER                  
046500                         KVAE-PCB KVAF-PCB                                
046600                         KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB              
046610                         WDK7-PCB WDB6-PCB                                
046700                                                                          
046800     PERFORM FA-SET-MSG-AND-HILIGHT                                       
046900     IF NOT WRONG-KEY                                                     
047000        PERFORM FB-MOVE-RESP-TO-MOD                                       
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 FA-SET-MSG-AND-HILIGHT SECTION.                                          
047500                                                                          
047600     MOVE RESP-IDMSG-ERROR  TO MCNV-IDMSG-ERROR                           
047700                               W-IDMSG-ERROR                              
047800     MOVE RESP-IDMSG-INFO   TO MCNV-IDMSG-INFO                            
047900     MOVE RESP-IDELMT-ERROR TO MCNV-IDELMT-ERROR                          
048000     MOVE MSGI-IDSPRAK      TO MCNV-IDSPRAK                               
048100                                                                          
048200     CALL WL01MCNV USING MCNV-AREA                                        
048300                                                                          
048400     MOVE MCNV-MFSINF       TO MOD-TEMFSINF                               
048500     MOVE MCNV-MFSFEL       TO MOD-TEMFSFEL                               
048510     IF WRONG-KEY                                                         
048520        MOVE REQU-IDARTNR-KEY  TO MOD-IDARTNR-UT                          
048530        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
048540        MOVE MFS-RENSA-FAELT   TO MOD-KDKRSTA-UT                          
048550                                  MOD-KDPERSTYP-UT                        
048560                                  MOD-IDPERSON-UT                         
048570                                  MOD-KDBEHX-UT                           
048580                                  MOD-IDTYP-UT                            
048590                                  MOD-IDDC-UT                             
048591                                  MOD-FLANNULL-UT                         
048592     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 FB-MOVE-RESP-TO-MOD SECTION.                                             
048900                                                                          
049000     MOVE +1                       TO INDX                                
049100     PERFORM UNTIL INDX > RESP-KVRADER                                    
049200       MOVE RESP-KDBEHX-LINE-ATTR(INDX)                                   
049300                                   TO MOD-KDBEHX-ATTR(INDX)               
049400                                                                          
049500       IF RESP-KDBEHX-UPDATE-LINE(INDX) = SPACE                           
049600         MOVE MFS-ERASE-FIELD      TO MOD-KDBEHX-UPDATE(INDX)             
049700       ELSE                                                               
049800         IF RESP-KDBEHX-UPDATE-LINE(INDX) = ALL '+'                       
049900           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
050000                                   TO MOD-KDBEHX-UPDATE(INDX)             
050100         ELSE                                                             
050200           MOVE RESP-KDBEHX-UPDATE-LINE(INDX)                             
050300                                   TO MOD-KDBEHX-UPDATE(INDX)             
050400         END-IF                                                           
050500       END-IF                                                             
050600                                                                          
050700       IF RESP-IDARTNR-LINE(INDX) = SPACE                                 
050800         MOVE MFS-ERASE-FIELD      TO MOD-IDARTNR-RAD(INDX)               
050900       ELSE                                                               
051000         IF RESP-IDARTNR-LINE(INDX) = ALL '+'                             
051100           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
051200                                   TO MOD-IDARTNR-RAD(INDX)               
051300         ELSE                                                             
051400           MOVE RESP-IDARTNR-LINE(INDX)                                   
051500                                   TO MOD-IDARTNR-RAD(INDX)               
051600         END-IF                                                           
051700       END-IF                                                             
051800                                                                          
051900       IF RESP-BEART-LINE(INDX) = SPACE                                   
052000         MOVE MFS-ERASE-FIELD      TO MOD-BEART-RAD(INDX)                 
052100       ELSE                                                               
052200         IF RESP-BEART-LINE(INDX) = ALL '+'                               
052300           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
052400                                   TO MOD-BEART-RAD(INDX)                 
052500         ELSE                                                             
052600           MOVE RESP-BEART-LINE(INDX)                                     
052700                                   TO MOD-BEART-RAD(INDX)                 
052800         END-IF                                                           
052900       END-IF                                                             
053000                                                                          
053100       IF RESP-TIREGDAT-LINE(INDX) = SPACE                                
053200         MOVE MFS-ERASE-FIELD      TO MOD-TIREGDAT-RAD(INDX)              
053300       ELSE                                                               
053400         IF RESP-TIREGDAT-LINE(INDX) = ALL '+'                            
053500           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
053600                                   TO MOD-TIREGDAT-RAD(INDX)              
053700         ELSE                                                             
053800           MOVE RESP-TIREGDAT-LINE(INDX)                                  
053900                                   TO MOD-TIREGDAT-RAD(INDX)              
054000         END-IF                                                           
054100       END-IF                                                             
054200                                                                          
054300       IF RESP-IDKR-LINE(INDX) = SPACE                                    
054400         MOVE MFS-ERASE-FIELD      TO MOD-IDKR-RAD(INDX)                  
054500                                      MOD-IDKR-SPAR(INDX)                 
054600       ELSE                                                               
054700         IF RESP-IDKR-LINE(INDX) = ALL '+'                                
054800           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
054900                                   TO MOD-IDKR-RAD(INDX)                  
055000                                      MOD-IDKR-SPAR(INDX)                 
055100         ELSE                                                             
055200           MOVE RESP-IDKR-LINE(INDX)                                      
055300                                   TO MOD-IDKR-RAD(INDX)                  
055400                                      MOD-IDKR-SPAR(INDX)                 
055500         END-IF                                                           
055600       END-IF                                                             
055700                                                                          
055800       IF RESP-IDLOPNRM-LINE(INDX) = SPACE                                
055900         MOVE MFS-ERASE-FIELD      TO MOD-IDLOPNRM-RAD(INDX)              
056000       ELSE                                                               
056100         IF RESP-IDLOPNRM-LINE(INDX) = ALL '+'                            
056200           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
056300                                   TO MOD-IDLOPNRM-RAD(INDX)              
056400         ELSE                                                             
056500           MOVE RESP-IDLOPNRM-LINE(INDX)                                  
056600                                   TO MOD-IDLOPNRM-RAD(INDX)              
056700         END-IF                                                           
056800       END-IF                                                             
056900                                                                          
057000       IF RESP-IDLEVNR-LINE(INDX) = SPACE                                 
057100         MOVE MFS-ERASE-FIELD      TO MOD-IDLEVNR-RAD(INDX)               
057200       ELSE                                                               
057300         IF RESP-IDLEVNR-LINE(INDX) = ALL '+'                             
057400           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
057500                                   TO MOD-IDLEVNR-RAD(INDX)               
057600         ELSE                                                             
057700           MOVE RESP-IDLEVNR-LINE(INDX)                                   
057800                                   TO MOD-IDLEVNR-RAD(INDX)               
057900         END-IF                                                           
058000       END-IF                                                             
058100                                                                          
058200       IF RESP-TYP-LINE(INDX) = SPACE                                     
058300         MOVE MFS-ERASE-FIELD      TO MOD-TYP-RAD(INDX)                   
058400       ELSE                                                               
058500         IF RESP-TYP-LINE(INDX) = ALL '+'                                 
058600           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
058700                                   TO MOD-TYP-RAD(INDX)                   
058800         ELSE                                                             
058900           MOVE RESP-TYP-LINE(INDX)                                       
059000                                   TO MOD-TYP-RAD(INDX)                   
059100         END-IF                                                           
059200       END-IF                                                             
059300                                                                          
059400       IF RESP-KVART-RET-LINE(INDX) = SPACE                               
059500         MOVE MFS-ERASE-FIELD      TO MOD-KVART-RET-RAD(INDX)             
059600       ELSE                                                               
059700         IF RESP-KVART-RET-LINE(INDX) = ALL '+'                           
059800           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
059900                                   TO MOD-KVART-RET-RAD(INDX)             
060000         ELSE                                                             
060100           MOVE RESP-KVART-RET-LINE(INDX)                                 
060200                                   TO MOD-KVART-RET-RAD(INDX)             
060300         END-IF                                                           
060400       END-IF                                                             
060500                                                                          
060600       IF RESP-BEKRBEH-LINE(INDX) = SPACE                                 
060700         MOVE MFS-ERASE-FIELD      TO MOD-BEKRBEH-RAD(INDX)               
060800       ELSE                                                               
060900         IF RESP-BEKRBEH-LINE(INDX) = ALL '+'                             
061000           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
061100                                   TO MOD-BEKRBEH-RAD(INDX)               
061200         ELSE                                                             
061300           MOVE RESP-BEKRBEH-LINE(INDX)                                   
061400                                   TO MOD-BEKRBEH-RAD(INDX)               
061500         END-IF                                                           
061600       END-IF                                                             
061700       ADD +1                      TO INDX                                
061800     END-PERFORM                                                          
061900                                                                          
062000     PERFORM UNTIL INDX > MAX-KVRADER                                     
062100       MOVE MFS-ERASE-FIELD        TO MOD-KDBEHX-UPDATE(INDX)             
062200                                      MOD-IDARTNR-RAD(INDX)               
062300                                      MOD-BEART-RAD(INDX)                 
062400                                      MOD-TIREGDAT-RAD(INDX)              
062500                                      MOD-IDKR-RAD(INDX)                  
062600                                      MOD-IDLOPNRM-RAD(INDX)              
062700                                      MOD-IDLEVNR-RAD(INDX)               
062800                                      MOD-TYP-RAD(INDX)                   
062900                                      MOD-KVART-RET-RAD(INDX)             
063000                                      MOD-BEKRBEH-RAD(INDX)               
063100       ADD +1                      TO INDX                                
063200     END-PERFORM                                                          
063300                                                                          
063400     IF RESP-IDDC-START = ALL '+'                                         
063500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDDC-ENTER                    
063600     ELSE                                                                 
063700       MOVE RESP-IDDC-START          TO MOD-IDDC-ENTER                    
063800     END-IF                                                               
063900     IF RESP-IDDC-NEXT = ALL '+'                                          
064000       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDDC-NEXT                     
064100     ELSE                                                                 
064200       MOVE RESP-IDDC-NEXT           TO MOD-IDDC-NEXT                     
064300     END-IF                                                               
064400     IF RESP-TIREGDAT-START = ALL '+'                                     
064500       MOVE MFS-ROER-EJ-FAELT        TO MOD-TIREGDAT-ENTER                
064600     ELSE                                                                 
064700       MOVE RESP-TIREGDAT-START      TO MOD-TIREGDAT-ENTER                
064800     END-IF                                                               
064900     IF RESP-TIREGDAT-NEXT = ALL '+'                                      
065000       MOVE MFS-ROER-EJ-FAELT        TO MOD-TIREGDAT-NEXT                 
065100     ELSE                                                                 
065200       MOVE RESP-TIREGDAT-NEXT       TO MOD-TIREGDAT-NEXT                 
065300     END-IF                                                               
065400     IF RESP-KDKRSTA-START = ALL '+'                                      
065500       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDKRSTA-ENTER                 
065600     ELSE                                                                 
065700       MOVE RESP-KDKRSTA-START       TO MOD-KDKRSTA-ENTER                 
065800     END-IF                                                               
065900     IF RESP-KDKRSTA-NEXT = ALL '+'                                       
066000       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDKRSTA-NEXT                  
066100     ELSE                                                                 
066200       MOVE RESP-KDKRSTA-NEXT        TO MOD-KDKRSTA-NEXT                  
066300     END-IF                                                               
066400     IF RESP-IDLOPNRM-START = ALL '+'                                     
066500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDLOPNRM-ENTER                
066600     ELSE                                                                 
066700       MOVE RESP-IDLOPNRM-START      TO MOD-IDLOPNRM-ENTER                
066800     END-IF                                                               
066900     IF RESP-IDLOPNRM-NEXT = ALL '+'                                      
067000       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDLOPNRM-NEXT                 
067100     ELSE                                                                 
067200       MOVE RESP-IDLOPNRM-NEXT       TO MOD-IDLOPNRM-NEXT                 
067300     END-IF                                                               
067400     IF RESP-IDFTG-START = ALL '+'                                        
067500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDFTG-ENTER                   
067600     ELSE                                                                 
067700       MOVE RESP-IDFTG-START         TO MOD-IDFTG-ENTER                   
067800     END-IF                                                               
067900     IF RESP-IDFTG-NEXT = ALL '+'                                         
068000       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDFTG-NEXT                    
068100     ELSE                                                                 
068200       MOVE RESP-IDFTG-NEXT          TO MOD-IDFTG-NEXT                    
068300     END-IF                                                               
068400     IF RESP-IDKR-START = ALL '+'                                         
068500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDKR-ENTER                    
068600     ELSE                                                                 
068700       MOVE RESP-IDKR-START          TO MOD-IDKR-ENTER                    
068800     END-IF                                                               
068900     IF RESP-IDKR-NEXT = ALL '+'                                          
069000       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDKR-NEXT                     
069100     ELSE                                                                 
069200       MOVE RESP-IDKR-NEXT           TO MOD-IDKR-NEXT                     
069300     END-IF                                                               
069400     IF RESP-IDARTNR-START = ALL '+'                                      
069500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDARTNR-ENTER                 
069600     ELSE                                                                 
069700       MOVE RESP-IDARTNR-START       TO MOD-IDARTNR-ENTER                 
069800     END-IF                                                               
069900     IF RESP-IDARTNR-NEXT = ALL '+'                                       
070000       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDARTNR-NEXT                  
070100     ELSE                                                                 
070200       MOVE RESP-IDARTNR-NEXT        TO MOD-IDARTNR-NEXT                  
070300     END-IF                                                               
070400     IF RESP-DAREGDAT-9KOMPL-START = ALL '+'                              
070500       MOVE MFS-ROER-EJ-FAELT        TO MOD-DAREGDAT-9KOMPL-ENTER         
070600     ELSE                                                                 
070700       MOVE RESP-DAREGDAT-9KOMPL-START TO                                 
070800                                        MOD-DAREGDAT-9KOMPL-ENTER         
070900     END-IF                                                               
071000     IF RESP-DAREGDAT-9KOMPL-NEXT = ALL '+'                               
071100       MOVE MFS-ROER-EJ-FAELT        TO MOD-DAREGDAT-9KOMPL-NEXT          
071200     ELSE                                                                 
071300       MOVE RESP-DAREGDAT-9KOMPL-NEXT TO MOD-DAREGDAT-9KOMPL-NEXT         
071400     END-IF                                                               
071500     IF RESP-IDLEVNR-START      = ALL '+'                                 
071600       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDLEVNR-ENTER                 
071700     ELSE                                                                 
071800       MOVE RESP-IDLEVNR-START       TO MOD-IDLEVNR-ENTER                 
071900     END-IF                                                               
072000     IF RESP-IDLEVNR-NEXT      = ALL '+'                                  
072100       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDLEVNR-NEXT                  
072200     ELSE                                                                 
072300       MOVE RESP-IDLEVNR-NEXT        TO MOD-IDLEVNR-NEXT                  
072400     END-IF                                                               
072500     IF RESP-KVKRKNTR-START = ALL '+'                                     
072600       MOVE MFS-ROER-EJ-FAELT        TO MOD-KVKRKNTR-ENTER                
072700     ELSE                                                                 
072800       MOVE RESP-KVKRKNTR-START      TO MOD-KVKRKNTR-ENTER                
072900     END-IF                                                               
073000     IF RESP-KVKRKNTR-NEXT = ALL '+'                                      
073100       MOVE MFS-ROER-EJ-FAELT        TO MOD-KVKRKNTR-NEXT                 
073200     ELSE                                                                 
073300       MOVE RESP-KVKRKNTR-NEXT       TO MOD-KVKRKNTR-NEXT                 
073400     END-IF                                                               
073500     .                                                                    
073600 G-INIT-REQU SECTION.                                                     
073700                                                                          
073800     MOVE MAX-KVRADER               TO REQU-KVRADER                       
073900                                                                          
074000     MOVE +1                        TO INDX                               
074100     PERFORM UNTIL INDX > MAX-INDX                                        
074200       MOVE MID-KDBEHX-UPDATE(INDX) TO                                    
074300                                    REQU-KDBEHX-UPDATE-LINE(INDX)         
074400       MOVE MID-IDKR-SPAR(INDX)     TO REQU-IDKR-LINE(INDX)               
074500       ADD +1                       TO INDX                               
074600     END-PERFORM                                                          
074700                                                                          
074800     MOVE '101'                     TO REQU-IDMSGVER                      
074900     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
075000     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
075100     .                                                                    
075200 H-HOPP-TILL-ANNAN-BILD SECTION.                                          
075300     MOVE NEJ                     TO KDBEHX-SW                            
075400                                     ALLT-SW                              
075500     MOVE JA                      TO HOPP-SW                              
075600     MOVE +1                      TO INDX                                 
075700     PERFORM UNTIL INDX > MAX-INDX OR KDBEHX-FOUND                        
075800       IF MID-KDBEHX-UPDATE(INDX) NOT = ALL '+'                           
075900         MOVE MID-IDKR-SPAR(INDX) TO ALT-IDKR-IN                          
076000         MOVE REQU-IDDC-KEY       TO ALT-IDDC-IN                          
076100       END-IF                                                             
076200       ADD +1                     TO INDX                                 
076300     END-PERFORM                                                          
076400     IF MFS-IDPFK = 'A'                                                   
076500       MOVE '6201'                TO ALT-IDTRANS                          
076600       MOVE 'W6T202  '            TO ALT-TRANSKOD                         
076700       PERFORM IMS-INSERT-ALTA-MSG                                        
076800     END-IF                                                               
076900     IF MFS-IDPFK = 'B'                                                   
077000       MOVE '6201'                TO ALT-IDTRANS                          
077100       MOVE 'W6T203  '            TO ALT-TRANSKOD                         
077200       PERFORM IMS-INSERT-ALTB-MSG                                        
077300     END-IF                                                               
077400     IF MFS-IDPFK = 'C'                                                   
077500       MOVE '6201'                TO ALT-IDTRANS                          
077600       MOVE 'W6T204  '            TO ALT-TRANSKOD                         
077700       PERFORM IMS-INSERT-ALTC-MSG                                        
077800     END-IF                                                               
077900     IF MFS-IDPFK = 'D'                                                   
078000       MOVE '6201'                TO ALT-IDTRANS                          
078100       MOVE 'W6T207  '            TO ALT-TRANSKOD                         
078200       PERFORM IMS-INSERT-ALTD-MSG                                        
078300     END-IF                                                               
078400     IF MFS-IDPFK = 'E'                                                   
078500       MOVE '6201'                TO ALT-IDTRANS                          
078600       MOVE 'W6T208  '            TO ALT-TRANSKOD                         
078700       PERFORM IMS-INSERT-ALTE-MSG                                        
078800     END-IF                                                               
078900     .                                                                    
079000                                                                          
079100 MFS-RENSA-FAELT-UT SECTION.                                              
079200                                                                          
079300*    --- ALLA UTDATA-FÄLT                                                 
079400*    --- INKL. BLÄDDRINGSNYCKLAR                                          
079500     MOVE MFS-RENSA-FAELT TO MOD-KDKRSTA-ENTER                            
079600                             MOD-KDKRSTA-NEXT                             
079700                             MOD-IDARTNR-ENTER                            
079800                             MOD-IDARTNR-NEXT                             
079900                             MOD-DAREGDAT-9KOMPL-ENTER                    
080000                             MOD-DAREGDAT-9KOMPL-NEXT                     
080100                             MOD-TIREGDAT-ENTER                           
080200                             MOD-TIREGDAT-NEXT                            
080300                             MOD-IDLOPNRM-ENTER                           
080400                             MOD-IDLOPNRM-NEXT                            
080500                             MOD-IDFTG-ENTER                              
080600                             MOD-IDFTG-NEXT                               
080700                             MOD-IDKR-ENTER                               
080800                             MOD-IDKR-NEXT                                
080900                             MOD-IDLEVNR-ENTER                            
081000                             MOD-IDLEVNR-NEXT                             
081100                             MOD-IDDC-ENTER                               
081200                             MOD-IDDC-NEXT                                
081300*                                                                         
081400     MOVE +1              TO INDX                                         
081500     PERFORM UNTIL INDX > MAX-INDX                                        
081600       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-RAD(INDX)                    
081700                                 MOD-BEART-RAD(INDX)                      
081800                                 MOD-TIREGDAT-RAD(INDX)                   
081900                                 MOD-IDKR-RAD(INDX)                       
082000                                 MOD-IDLOPNRM-RAD(INDX)                   
082100                                 MOD-IDLEVNR-RAD(INDX)                    
082200                                 MOD-TYP-RAD(INDX)                        
082300                                 MOD-KVART-RET-RAD(INDX)                  
082400                                 MOD-BEKRBEH-RAD(INDX)                    
082500                                 MOD-IDKR-SPAR(INDX)                      
082600       ADD +1                 TO INDX                                     
082700     END-PERFORM                                                          
082800     .                                                                    
082900     SKIP2                                                                
083000 MFS-RENSA-FAELT-IN SECTION.                                              
083100                                                                          
083200*    --- ALLA INDATA-FÄLT                                                 
083300     MOVE +1              TO INDX                                         
083400     PERFORM UNTIL INDX > MAX-INDX                                        
083500       MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-UPDATE(INDX)                    
083600       ADD +1             TO INDX                                         
083700     END-PERFORM                                                          
083800     .                                                                    
083900     EJECT                                                                
084000* --- IMS SEKTIONER ---                                                   
084100 IMS-GET-MSG SECTION.                                                     
084200                                                                          
084300     MOVE '  QC' TO GODK-STATUSKODER                                      
084400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
084500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     SKIP3                                                                
084900 IMS-INSERT-MSG SECTION.                                                  
085000                                                                          
085100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
085200       MOVE '0' TO MFS-KDHUVOMR                                           
085300     END-IF                                                               
085400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085500     MOVE SPACES  TO GODK-STATUSKODER                                     
085600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000     EJECT                                                                
086100 IMS-INSERT-ALTA-MSG SECTION.                                             
086200                                                                          
086300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
086400       MOVE '0' TO MFS-KDHUVOMR                                           
086500     END-IF                                                               
086600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
086700     MOVE SPACE TO GODK-STATUSKODER                                       
086800     CALL CBLTDLI USING ISRT ALTA-PCB ALT-MSG-IO-AREA                     
086900     MOVE ALTA-STATUS-CODE TO STATUS-WS                                   
087000     PERFORM IMS-STATUSKONTROLL                                           
087100     .                                                                    
087200     EJECT                                                                
087300 IMS-INSERT-ALTB-MSG SECTION.                                             
087400                                                                          
087500     IF ENGLISH-TEXT                                                      
087600       MOVE 'N' TO MFS-KDHUVOMR                                           
087700     END-IF                                                               
087800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
087900     MOVE SPACE TO GODK-STATUSKODER                                       
088000     CALL CBLTDLI USING ISRT ALTB-PCB ALT-MSG-IO-AREA                     
088100     MOVE ALTB-STATUS-CODE TO STATUS-WS                                   
088200     PERFORM IMS-STATUSKONTROLL                                           
088300     .                                                                    
088400     EJECT                                                                
088500 IMS-INSERT-ALTC-MSG SECTION.                                             
088600                                                                          
088700     IF ENGLISH-TEXT                                                      
088800       MOVE 'N' TO MFS-KDHUVOMR                                           
088900     END-IF                                                               
089000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
089100     MOVE SPACE TO GODK-STATUSKODER                                       
089200     CALL CBLTDLI USING ISRT ALTC-PCB ALT-MSG-IO-AREA                     
089300     MOVE ALTC-STATUS-CODE TO STATUS-WS                                   
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     EJECT                                                                
089700 IMS-INSERT-ALTD-MSG SECTION.                                             
089800                                                                          
089900     IF ENGLISH-TEXT                                                      
090000       MOVE 'N' TO MFS-KDHUVOMR                                           
090100     END-IF                                                               
090200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090300     MOVE SPACE TO GODK-STATUSKODER                                       
090400     CALL CBLTDLI USING ISRT ALTD-PCB ALT-MSG-IO-AREA                     
090500     MOVE ALTD-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-INSERT-ALTE-MSG SECTION.                                             
091000                                                                          
091100     IF ENGLISH-TEXT                                                      
091200       MOVE 'N' TO MFS-KDHUVOMR                                           
091300     END-IF                                                               
091400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
091500     MOVE SPACE TO GODK-STATUSKODER                                       
091600     CALL CBLTDLI USING ISRT ALTE-PCB ALT-MSG-IO-AREA                     
091700     MOVE ALTE-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000 IMS-STATUSKONTROLL SECTION.                                              
092100                                                                          
092200     SET STATUS-IX TO 1                                                   
092300     SEARCH GODK-STATUS                                                   
092400       AT END                                                             
092500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
092600         DELIMITED BY SIZE INTO FELTEXT                                   
092700         CALL FELLOG                                                      
092800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
092900     END-SEARCH                                                           
093000     .                                                                    
