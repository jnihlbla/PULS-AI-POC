000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2712800.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   DEC 2002.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*                                                                         
001100*        TAR EMOT EN BESTÄLLNING FRÅN 2348 MED URVALSPARAMETRAR           
001200*        OCH TAR FRAM DE ARTIKLAR SOM MAN FÅR TRÄFF PÅ                    
001300*        DOCK MAX 1000 ST                                                 
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          ---                                                            
002800     SELECT W20348                     ASSIGN TO W27128D1.                
002900     SKIP2                                                                
003000*          ---                                                            
003100     SELECT W27127                     ASSIGN TO W27128D2.                
003200     SKIP2                                                                
003300*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003400     SELECT W27128                     ASSIGN TO W27128D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W20348                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  W20348-POST             PIC X(80).                                   
004500     SKIP3                                                                
004600 FD  W27127                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W27126      -L.                                                
005100     SKIP3                                                                
005200 FD  W27128                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY W27128 -PRE UT-     -L.                                   
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
006000*    -COPY WY2000W1                                                       
006100     SKIP3                                                                
006200*    -COPY WY2000W2                                                       
006300     SKIP3                                                                
006400 77  IDPGM                       PIC X(8)    VALUE 'W2712800'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  YES                         PIC X       VALUE 'Y'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  FLENG                       PIC S9(4)   COMP.                        
006900                                                                          
007000 01  ARBETSFALT.                                                          
007100     03 IX                       PIC 9(2)    VALUE ZERO.                  
007200     03 IX-FROM                  PIC 9(4)    VALUE ZERO.                  
007300     03 IX-BEMOD                 PIC 9(4)    VALUE ZERO.                  
007400     03 INDX                     PIC 9(2)    VALUE ZERO.                  
007500     03 MAX-INDX                 PIC 9(2)    VALUE 12.                    
007600     03 W-BEMODELL               PIC X(15) VALUE SPACE.                   
007700     SKIP2                                                                
007800 01  WS-AREA-RED.                                                         
007900     03  WS-IDPERSON-BUY-NUM2 PIC 9(3)         VALUE ZERO.                
007910     03  WS-IDPERSON-BUY-NUM3 PIC 9(3)         VALUE ZERO.                
007920     03  WS-IDPERSON-BUY-NUM4 PIC 9(3)         VALUE ZERO.                
008000     03  WS-IDPERSON-BUY-FOM-NUM PIC 9(3)         VALUE ZERO.             
008100     03  WS-IDPERSON-BUY-TOM-NUM PIC 9(3)         VALUE ZERO.             
008200     03  WS-KVPB-REF-FOM-NUM PIC 9(6)V9       VALUE ZERO.                 
008300     03  WS-KVPB-REF-TOM-NUM PIC 9(6)V9       VALUE ZERO.                 
008400     03  WS-ADLAGOMR-NUM     PIC 9(3)         VALUE ZERO.                 
008500     03  WS-ADGANG-NUM       PIC 9(3)         VALUE ZERO.                 
008600     03  WS-ADPLATS-NUM      PIC 9(5)         VALUE ZERO.                 
008700     03  WS-KVLS-NUM         PIC 9(7)         VALUE ZERO.                 
008800     03  WS-TIFINLV-NUM      PIC 9(5)         VALUE ZERO.                 
008900     03  WS-VKART-NUM        PIC 9(7)         VALUE ZERO.                 
009000     03  WS-VLARTNTO-NUM     PIC 9(9)         VALUE ZERO.                 
009100     03  WS-ADPLATS-FOM-NUM  PIC 9(5)         VALUE ZERO.                 
009200     03  WS-ADPLATS-TOM-NUM  PIC 9(5)         VALUE ZERO.                 
009300     03  WS-KDERS-NUM        PIC 9(3)         VALUE ZERO.                 
009400     03  WS-KDPRODSL-NUM     PIC 9(3)         VALUE ZERO.                 
009500     03  WS-IDFKNGRP-FOM-NUM PIC 9(5)         VALUE ZERO.                 
009600     03  WS-IDFKNGRP-TOM-NUM PIC 9(5)         VALUE ZERO.                 
009700     03  WS-TIREFEFT-NUM     PIC 9(6)         VALUE ZERO.                 
009800     03  WS-SUPERWEEK-NUM    PIC 9(6)         VALUE ZERO.                 
009900     03  WS-IDREFTAB-NUM     PIC 9(1)         VALUE ZERO.                 
010000     03  WS-PRARTSTD-NUM     PIC 9(9)         VALUE ZERO.                 
010100     03  WS-RED-KVPB-REF     PIC Z(4)9.9      VALUE ZERO.                 
010200     03  WS-RED-KVPB-REF-NUM PIC 9(6)V9       VALUE ZERO.                 
010300     03  WS-PRISRAD-NUM      PIC 9(2)         VALUE ZERO.                 
010400     03  WS-KLASS.                                                        
010500         05  WS-PRISRAD-RED  PIC Z(2)         VALUE ZERO.                 
010600         05  WS-PBRAD-RED    PIC X(1)         VALUE SPACE.                
010700     03  WS-KLASS-IN.                                                     
010800         05  WS-PRISRAD-IN   PIC 9(2)         VALUE ZERO.                 
010900         05  WS-PBRAD-IN     PIC X(1)         VALUE SPACE.                
011000     03  WS-KLASS-X.                                                      
011100         05  WS-PRISRAD-RED-X                                             
011200                             PIC 9(2)         VALUE ZERO.                 
011300         05  WS-PBRAD-RED-X  PIC X(1)         VALUE SPACE.                
011400     03  WS-ANTAL-TRAEFF     PIC 9(9)         VALUE ZERO.                 
011500                                                                          
011600 77 SW-TRAEFF                       PIC X   VALUE 'N'.                    
011700     88  TRAEFF                             VALUE 'J'.                    
011800                                                                          
011900 77  SEASON-SW                      PIC X   VALUE 'N'.                    
012000     88  SEASON-OK                          VALUE 'J'                     
012100                                                  'Y'.                    
012200     88  SEASON-WRONG                       VALUE 'N'.                    
012300                                                                          
012400 77  BEMODELL-SW                      PIC X   VALUE 'N'.                  
012500     88  BEMODELL-OK                          VALUE 'J'                   
012600                                                  'Y'.                    
012700     88  BEMODELL-WRONG                       VALUE 'N'.                  
012800                                                                          
012900 77  BEMODELL-TRAEFF-SW                      PIC X   VALUE 'N'.           
013000     88  BEMODELL-TRAEFF                             VALUE 'J'.           
013100                                                                          
013200 01  ERRTEXT.                                                             
013300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
013400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
013500                                                                          
013600 77  W20348-EOF-SW               PIC X       VALUE 'N'.                   
013700     88  END-OF-W20348                       VALUE 'Y'.                   
013800                                                                          
013900 77  W27127-EOF-SW               PIC X       VALUE 'N'.                   
014000     88  END-OF-W27127                       VALUE 'Y'.                   
014100     EJECT                                                                
014200*                                                                         
014300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014400 01  FILLER REDEFINES DAGENS-DATUM.                                       
014500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014800     SKIP2                                                                
014900 01  WS-AREA-PARM1.                                                       
015000     03  WS-IDLAND-SPR       PIC X(2)  VALUE SPACE.                       
015100     03  WS-IDDC             PIC X(2)         VALUE SPACE.                
015200     03  WS-IDPERSON-BUY-FOM PIC X(3)         VALUE SPACE.                
015300     03  WS-IDPERSON-BUY-TOM PIC X(3)         VALUE SPACE.                
015400     03  WS-IDPERSON-BUY2    PIC X(3)         VALUE SPACE.                
015500     03  WS-IDPERSON-BUY3    PIC X(3)         VALUE SPACE.                
015600     03  WS-IDPERSON-BUY4    PIC X(3)         VALUE SPACE.                
015700     03  WS-IDPROJ-GRP OCCURS 3.                                          
015800         05  WS-IDPROJ       PIC X(4)  VALUE SPACE.                       
015900     03  WS-IDFKNGRP-FOM     PIC X(4)         VALUE SPACE.                
016000     03  WS-IDFKNGRP-TOM     PIC X(4)         VALUE SPACE.                
016100     03  WS-KVPB-REF-FOM     PIC X(7)         VALUE SPACE.                
016200     03  WS-KVPB-REF-TOM     PIC X(7)         VALUE SPACE.                
016300     03  WS-ADLAGOMR         PIC X(2)         VALUE SPACE.                
016400     03  WS-ADGANG           PIC X(2)         VALUE SPACE.                
016500     03  WS-ADPLATS-FOM      PIC X(5)         VALUE SPACE.                
016600     03  WS-KDERS            PIC X(3)         VALUE SPACE.                
016700     03  WS-KVLS-TKN         PIC X(1)         VALUE SPACE.                
016800     03  WS-KVLS             PIC X(7)         VALUE SPACE.                
016900     03  WS-TIREFEFT-TKN     PIC X(1)         VALUE SPACE.                
017000     03  WS-TIREFEFT         PIC X(6)         VALUE SPACE.                
017100*    03  FILLER              PIC X(8)         VALUE SPACE.                
017200                                                                          
017300 01  WS-AREA-PARM2.                                                       
017400     03  WS-BEART            PIC X(25)        VALUE SPACE.                
017500     03  WS-KDPRODSL         PIC X(2)         VALUE SPACE.                
017600     03  FILLER              PIC X(53)        VALUE SPACE.                
017700*    03  FILLER              PIC X(55)        VALUE SPACE.                
017800                                                                          
017900 01  WS-AREA-PARM3.                                                       
018000     05  WS-PRISRAD          PIC X(2)  VALUE SPACE.                       
018100     05  WS-PBRAD            PIC X(1)  VALUE SPACE.                       
018200     05  WS-IDREFTAB         PIC X(1)  VALUE SPACE.                       
018300     05  WS-FLKVROS          PIC X(1)  VALUE SPACE.                       
018400     05  WS-FLONORDER        PIC X(1)  VALUE SPACE.                       
018500     05  WS-FLAK-DC          PIC X(1)  VALUE SPACE.                       
018600     05  WS-FLASEAS          PIC X(1)  VALUE SPACE.                       
018700     05  WS-IDLEVNR-CDC      PIC X(5)  VALUE SPACE.                       
018800     05  WS-IDLEVNR-DC       PIC X(5)  VALUE SPACE.                       
018900     05  WS-FLREFILL         PIC X(1)  VALUE SPACE.                       
019000     05  WS-FLREFBEO         PIC X(1)  VALUE SPACE.                       
019100     05  WS-VKART-TKN        PIC X(1)  VALUE SPACE.                       
019200     05  WS-VKART            PIC X(7)  VALUE SPACE.                       
019300     05  WS-VLARTNTO-TKN     PIC X(1)  VALUE SPACE.                       
019400     05  WS-VLARTNTO         PIC X(9)  VALUE SPACE.                       
019500     05  WS-ADPLATS-TOM      PIC X(5)  VALUE SPACE.                       
019600     05  WS-PRARTSTD-TKN     PIC X(1)  VALUE SPACE.                       
019700     05  WS-PRARTSTD         PIC X(9)  VALUE SPACE.                       
019800     05  WS-TIFINLV-TKN      PIC X(1)  VALUE SPACE.                       
019900     05  WS-TIFINLV          PIC X(5)  VALUE SPACE.                       
020000     05  WS-BEMODELL         PIC X(15) VALUE SPACE.                       
020100     05  WS-KDREFSTA         PIC X(1)  VALUE SPACE.                       
020200     05  WS-SUPERWEEK-TKN    PIC X(1)  VALUE SPACE.                       
020300     05  WS-SUPERWEEK        PIC X(3)  VALUE SPACE.                       
020400     05  WS-FLFLYG           PIC X(1)  VALUE SPACE.                       
020500                                                                          
020600       EJECT                                                              
020700 01  GENERAL-SUBPROGRAM.                                                  
020800*                                                                         
020900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
021300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
021400     EJECT                                                                
021500*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
021600*01  -COPY WDECAREA                                                       
021700     EJECT                                                                
021800                                                                          
021900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022100                                                                          
022200*    --- PARAMETRAR TILL POSTSUM                                          
022300*                                                                         
022400*01  -COPY W0005   -PRE  POSTSUM-                                         
022500     EJECT                                                                
022600*                                                                         
022700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27128'.              
022800     SKIP2                                                                
022900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
023000     EJECT                                                                
023100 01  IN-AREA-START               PIC X(24)   VALUE                        
023200                                             'IN-AREA-START'.             
023300     SKIP2                                                                
023400                                                                          
023500*01  AREA -COPY W27126     -PRE IN-                                       
023600*                                                                         
023700     EJECT                                                                
023800 01  UT-AREA-START               PIC X(24)   VALUE                        
023900                                             'UT-AREA-START'.             
024000     SKIP2                                                                
024100                                                                          
024200*01  AREA -COPY W27128     -PRE UT-                                       
024300*                                                                         
024400     EJECT                                                                
024500 PROCEDURE DIVISION.                                                      
024600                                                                          
024700     PERFORM A-INIT                                                       
024800     PERFORM S01-READ-W27127                                              
024900                                                                          
025000     PERFORM UNTIL END-OF-W27127                                          
025100                                                                          
025200       PERFORM B-URVAL                                                    
025300                                                                          
025400       IF TRAEFF                                                          
025500         ADD 1               TO WS-ANTAL-TRAEFF                           
025600         PERFORM C-BEHANDLA-UTPOST                                        
025700       END-IF                                                             
025800                                                                          
025900       PERFORM S01-READ-W27127                                            
026000                                                                          
026100     END-PERFORM                                                          
026200                                                                          
026300     PERFORM Z-FINIT                                                      
026400                                                                          
026500     IF  WS-ANTAL-TRAEFF > 50000                                          
026600       MOVE 1    TO RETURN-CODE                                           
026700     ELSE                                                                 
026800       MOVE ZERO TO RETURN-CODE                                           
026900     END-IF                                                               
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027500     OPEN INPUT W20348                                                    
027600                W27127                                                    
027700                                                                          
027800     OPEN OUTPUT W27128                                                   
027900                                                                          
028000     ACCEPT DAGENS-DATUM FROM DATE                                        
028100                                                                          
028200**********************************************                            
028300*    HÄR LÄSES PARAMETRARNA FRÅN BILD 2348 IN                             
028400**********************************************                            
028500                                                                          
028600     PERFORM S02-LAES-W20348                                              
028700     IF NOT END-OF-W20348                                                 
028800         MOVE W20348-POST TO WS-AREA-PARM1                                
028900     ELSE                                                                 
029000       MOVE 'PARAMETERRAD 1 FRÅN SOP SAKNAS '                             
029100             TO ERRTEXT-STR                                               
029200       DISPLAY ERRTEXT                                                    
029300       PERFORM S99-ABEND                                                  
029400     END-IF                                                               
029500     PERFORM S02-LAES-W20348                                              
029600     IF NOT END-OF-W20348                                                 
029700         MOVE W20348-POST TO WS-AREA-PARM2                                
029800     ELSE                                                                 
029900       MOVE 'PARAMETERRAD 2 FRÅN SOP SAKNAS '                             
030000             TO ERRTEXT-STR                                               
030100       DISPLAY ERRTEXT                                                    
030200       PERFORM S99-ABEND                                                  
030300     END-IF                                                               
030400     PERFORM S02-LAES-W20348                                              
030500     IF NOT END-OF-W20348                                                 
030600         MOVE W20348-POST TO WS-AREA-PARM3                                
030700     ELSE                                                                 
030800       MOVE 'PARAMETERRAD 3 FRÅN SOP SAKNAS '                             
030900             TO ERRTEXT-STR                                               
031000       DISPLAY ERRTEXT                                                    
031100       PERFORM S99-ABEND                                                  
031200     END-IF                                                               
031300                                                                          
031400     MOVE SPACE              TO UT-POST                                   
031500     MOVE '001'              TO UT-IDPTYP                                 
031600     MOVE WS-IDDC            TO UT-IDDC                                   
031700     MOVE WS-FLKVROS         TO UT-FLKVROS                                
031800     MOVE WS-IDPERSON-BUY-FOM  TO UT-IDPERSON-BUY-FOM                     
031900     MOVE WS-IDPERSON-BUY-TOM  TO UT-IDPERSON-BUY-TOM                     
032000     MOVE WS-IDPERSON-BUY2   TO UT-IDPERSON-BUY2                          
032100     MOVE WS-IDPERSON-BUY3   TO UT-IDPERSON-BUY3                          
032200     MOVE WS-IDPERSON-BUY4   TO UT-IDPERSON-BUY4                          
032300     MOVE WS-FLONORDER       TO UT-FLONORDER                              
032400     MOVE WS-FLAK-DC         TO UT-FLAK-DC                                
032500     MOVE WS-FLASEAS         TO UT-FLASEAS                                
032600     MOVE WS-IDPROJ (1)      TO UT-IDPROJ (1)                             
032700     MOVE WS-IDPROJ (2)      TO UT-IDPROJ (2)                             
032800     MOVE WS-IDPROJ (3)      TO UT-IDPROJ (3)                             
032900     MOVE WS-IDLEVNR-CDC     TO UT-IDLEVNR-CDC                            
033000     MOVE WS-KDPRODSL        TO UT-KDPRODSL                               
033100     MOVE WS-IDLEVNR-DC      TO UT-IDLEVNR-DC                             
033200     MOVE WS-IDFKNGRP-FOM    TO UT-IDFKNGRP-FOM                           
033300     MOVE WS-IDFKNGRP-TOM    TO UT-IDFKNGRP-TOM                           
033400     MOVE WS-FLREFILL        TO UT-FLREFILL                               
033500     MOVE WS-PRISRAD         TO UT-PRISRAD                                
033600     MOVE WS-PBRAD           TO UT-PBRAD                                  
033700     MOVE WS-FLREFBEO        TO UT-FLREFBEO                               
033800     MOVE WS-IDREFTAB        TO UT-IDREFTAB                               
033900     MOVE WS-VKART-TKN       TO UT-VKART-TKN                              
034000     MOVE WS-VKART           TO UT-VKART                                  
034100     MOVE WS-KVPB-REF-FOM    TO UT-KVPB-REF-FOM                           
034200     MOVE WS-KVPB-REF-TOM    TO UT-KVPB-REF-TOM                           
034300     MOVE WS-VLARTNTO-TKN    TO UT-VLARTNTO-TKN                           
034400     MOVE WS-VLARTNTO        TO UT-VLARTNTO                               
034500     MOVE WS-ADLAGOMR        TO UT-ADLAGOMR                               
034600     MOVE WS-ADGANG          TO UT-ADGANG                                 
034700     MOVE WS-ADPLATS-FOM     TO UT-ADPLATS-FOM                            
034800     MOVE WS-ADPLATS-TOM     TO UT-ADPLATS-TOM                            
034900     MOVE WS-KDERS (2:2)     TO UT-KDERS                                  
035000     MOVE WS-PRARTSTD-TKN    TO UT-PRARTSTD-TKN                           
035100     MOVE WS-PRARTSTD        TO UT-PRARTSTD                               
035200     MOVE WS-KVLS-TKN        TO UT-KVLS-TKN                               
035300     MOVE WS-KVLS            TO UT-KVLS                                   
035400     MOVE WS-TIFINLV-TKN     TO UT-TIFINLV-TKN                            
035500     MOVE WS-TIFINLV         TO UT-TIFINLV                                
035600     MOVE WS-TIREFEFT-TKN    TO UT-TIREFEFT-TKN                           
035700     MOVE WS-TIREFEFT        TO UT-TIREFEFT                               
035800     MOVE WS-BEART           TO UT-BEART                                  
035900     MOVE WS-BEMODELL        TO UT-BEMODELL                               
036000     MOVE WS-KDREFSTA        TO UT-KDREFSTA                               
036100     MOVE WS-SUPERWEEK-TKN   TO UT-SUPERWEEK-TKN                          
036200     MOVE WS-SUPERWEEK       TO UT-SUPERWEEK                              
036300     MOVE WS-FLFLYG          TO UT-FLFLYG                                 
036400                                                                          
036500     PERFORM S11-SKRIV-W27128                                             
036600     .                                                                    
036700     EJECT                                                                
036800 B-URVAL SECTION.                                                         
036900                                                                          
037000     MOVE JA                 TO SW-TRAEFF                                 
037100                                                                          
037200     IF WS-IDDC > SPACE                                                   
037300       IF WS-IDDC NOT = IN-IDDC                                           
037400         MOVE NEJ            TO SW-TRAEFF                                 
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     IF WS-FLKVROS > SPACE                                                
037900       IF WS-FLKVROS = JA                                                 
038000       OR WS-FLKVROS = YES                                                
038100         IF (IN-KVROS-DAG + IN-KVROS-BULK) = ZERO                         
038200           MOVE NEJ          TO SW-TRAEFF                                 
038300         END-IF                                                           
038400       END-IF                                                             
038500       IF WS-FLKVROS = NEJ                                                
038600         IF (IN-KVROS-DAG + IN-KVROS-BULK) > ZERO                         
038700           MOVE NEJ          TO SW-TRAEFF                                 
038800         END-IF                                                           
038900       END-IF                                                             
039000     END-IF                                                               
039100                                                                          
039200*    IF  WS-IDPERSON-BUY > SPACE                                          
039300*      MOVE WS-IDPERSON-BUY  TO WS-IDPERSON-BUY-NUM                       
039400*      IF  WS-IDPERSON-BUY-NUM NOT = IN-IDPERSON-BUY                      
039500*        MOVE NEJ            TO SW-TRAEFF                                 
039600*      END-IF                                                             
039700*    END-IF                                                               
039800                                                                          
039900                                                                          
040000     IF  WS-IDPERSON-BUY-FOM > SPACE                                      
040100     AND WS-IDPERSON-BUY-TOM > SPACE                                      
040200       MOVE WS-IDPERSON-BUY-FOM TO WS-IDPERSON-BUY-FOM-NUM                
040300       MOVE WS-IDPERSON-BUY-TOM TO WS-IDPERSON-BUY-TOM-NUM                
040400       IF  IN-IDPERSON-BUY >= WS-IDPERSON-BUY-FOM-NUM                     
040500       AND IN-IDPERSON-BUY <= WS-IDPERSON-BUY-TOM-NUM                     
040600         CONTINUE                                                         
040700       ELSE                                                               
040800         MOVE NEJ            TO SW-TRAEFF                                 
040900       END-IF                                                             
041000     END-IF                                                               
041100                                                                          
041200     IF  WS-IDPERSON-BUY2 > SPACE                                         
041300     AND WS-IDPERSON-BUY3 > SPACE                                         
041310     AND WS-IDPERSON-BUY4 > SPACE                                         
041400       MOVE WS-IDPERSON-BUY2  TO WS-IDPERSON-BUY-NUM2                     
041410       MOVE WS-IDPERSON-BUY3  TO WS-IDPERSON-BUY-NUM3                     
041420       MOVE WS-IDPERSON-BUY4  TO WS-IDPERSON-BUY-NUM4                     
041500       IF  WS-IDPERSON-BUY-NUM2 NOT = IN-IDPERSON-BUY                     
041510       AND WS-IDPERSON-BUY-NUM3 NOT = IN-IDPERSON-BUY                     
041520       AND WS-IDPERSON-BUY-NUM4 NOT = IN-IDPERSON-BUY                     
041600         MOVE NEJ            TO SW-TRAEFF                                 
041700       END-IF                                                             
041701     ELSE                                                                 
041710       IF WS-IDPERSON-BUY2 > SPACE                                        
041720       AND WS-IDPERSON-BUY3 > SPACE                                       
041721       AND WS-IDPERSON-BUY4 = SPACE                                       
041730         MOVE WS-IDPERSON-BUY2 TO WS-IDPERSON-BUY-NUM2                    
041740         MOVE WS-IDPERSON-BUY3 TO WS-IDPERSON-BUY-NUM3                    
041750         IF  WS-IDPERSON-BUY-NUM2 NOT = IN-IDPERSON-BUY                   
041760         AND WS-IDPERSON-BUY-NUM3 NOT = IN-IDPERSON-BUY                   
041770           MOVE NEJ          TO SW-TRAEFF                                 
041780         END-IF                                                           
041781       ELSE                                                               
041782         IF WS-IDPERSON-BUY2 > SPACE                                      
041783         AND WS-IDPERSON-BUY3 = SPACE                                     
041784         AND WS-IDPERSON-BUY4 = SPACE                                     
041785           MOVE WS-IDPERSON-BUY2 TO WS-IDPERSON-BUY-NUM2                  
041786           IF WS-IDPERSON-BUY-NUM2 NOT = IN-IDPERSON-BUY                  
041788             MOVE NEJ        TO SW-TRAEFF                                 
041789           END-IF                                                         
041790         END-IF                                                           
041791       END-IF                                                             
041810     END-IF                                                               
041900                                                                          
043200                                                                          
043400     IF WS-FLONORDER > SPACE                                              
043500       IF WS-FLONORDER = JA                                               
043600       OR WS-FLONORDER = YES                                              
043700         IF (IN-KVBEART   + IN-KVAKS-PAV) = ZERO                          
043800           MOVE NEJ          TO SW-TRAEFF                                 
043900         END-IF                                                           
044000       END-IF                                                             
044100       IF WS-FLONORDER = NEJ                                              
044200         IF (IN-KVBEART   + IN-KVAKS-PAV) > ZERO                          
044300           MOVE NEJ          TO SW-TRAEFF                                 
044400         END-IF                                                           
044500       END-IF                                                             
044600     END-IF                                                               
044700                                                                          
044800     IF  WS-IDPROJ (1) > SPACE                                            
044900       IF  WS-IDPROJ (1) = IN-IDPROJ                                      
045000         CONTINUE                                                         
045100       ELSE                                                               
045200         MOVE NEJ            TO SW-TRAEFF                                 
045300       END-IF                                                             
045400     END-IF                                                               
045500                                                                          
045600     IF  WS-IDPROJ (2) > SPACE                                            
045700       IF  WS-IDPROJ (2) = IN-IDPROJ                                      
045800         CONTINUE                                                         
045900       ELSE                                                               
046000         MOVE NEJ            TO SW-TRAEFF                                 
046100       END-IF                                                             
046200     END-IF                                                               
046300                                                                          
046400     IF  WS-IDPROJ (3) > SPACE                                            
046500       IF  WS-IDPROJ (3) = IN-IDPROJ                                      
046600         CONTINUE                                                         
046700       ELSE                                                               
046800         MOVE NEJ            TO SW-TRAEFF                                 
046900       END-IF                                                             
047000     END-IF                                                               
047100                                                                          
047200     IF WS-FLAK-DC > SPACE                                                
047300       IF WS-FLAK-DC = JA                                                 
047400       OR WS-FLAK-DC = YES                                                
047500         IF IN-KVAKS-SDC = ZERO                                           
047600           MOVE NEJ          TO SW-TRAEFF                                 
047700         END-IF                                                           
047800       END-IF                                                             
047900       IF WS-FLAK-DC = NEJ                                                
048000         IF IN-KVAKS-SDC > ZERO                                           
048100           MOVE NEJ          TO SW-TRAEFF                                 
048200         END-IF                                                           
048300       END-IF                                                             
048400     END-IF                                                               
048500                                                                          
048600     IF WS-FLASEAS > SPACE                                                
048700        MOVE NEJ             TO SEASON-SW                                 
048800        MOVE +1 TO INDX                                                   
048900        PERFORM UNTIL INDX > MAX-INDX                                     
049000          IF IN-RESEASON (INDX) NOT = 1.00                                
049100             MOVE YES TO SEASON-SW                                        
049200          END-IF                                                          
049300          ADD +1 TO INDX                                                  
049400        END-PERFORM                                                       
049500        IF (WS-FLASEAS = JA                                               
049600        OR WS-FLASEAS = YES)                                              
049700        AND SEASON-OK                                                     
049800          CONTINUE                                                        
049900        ELSE                                                              
050000          IF WS-FLASEAS = NEJ                                             
050100          AND SEASON-WRONG                                                
050200            CONTINUE                                                      
050300          ELSE                                                            
050400            MOVE NEJ         TO SW-TRAEFF                                 
050500          END-IF                                                          
050600        END-IF                                                            
050700     END-IF                                                               
050800                                                                          
050900     IF WS-IDLEVNR-CDC > SPACE                                            
051000       IF WS-IDLEVNR-CDC NOT = IN-IDLEVNR-CDC                             
051100         MOVE NEJ            TO SW-TRAEFF                                 
051200       END-IF                                                             
051300     END-IF                                                               
051400                                                                          
051500     IF  WS-KDPRODSL > SPACE                                              
051600       MOVE WS-KDPRODSL      TO WS-KDPRODSL-NUM                           
051700       IF  WS-KDPRODSL-NUM NOT = IN-KDPRODSL                              
051800         MOVE NEJ            TO SW-TRAEFF                                 
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200     IF WS-IDLEVNR-DC > SPACE                                             
052300       IF WS-IDLEVNR-DC NOT = IN-IDLEVNR-DC                               
052400         MOVE NEJ            TO SW-TRAEFF                                 
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800     IF  WS-IDFKNGRP-FOM > SPACE                                          
052900     AND WS-IDFKNGRP-TOM > SPACE                                          
053000       MOVE WS-IDFKNGRP-FOM  TO WS-IDFKNGRP-FOM-NUM                       
053100       MOVE WS-IDFKNGRP-TOM  TO WS-IDFKNGRP-TOM-NUM                       
053200       IF  IN-IDFKNGRP     >= WS-IDFKNGRP-FOM-NUM                         
053300       AND IN-IDFKNGRP     <= WS-IDFKNGRP-TOM-NUM                         
053400         CONTINUE                                                         
053500       ELSE                                                               
053600         MOVE NEJ            TO SW-TRAEFF                                 
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000     IF WS-FLREFILL > SPACE                                               
054100       IF   WS-FLREFILL = IN-FLREFILL                                     
054200       OR ((WS-FLREFILL = JA                                              
054300       OR   WS-FLREFILL = YES)                                            
054400       AND (IN-FLREFILL = JA                                              
054500       OR   IN-FLREFILL = YES))                                           
054600         CONTINUE                                                         
054700       ELSE                                                               
054800         MOVE NEJ            TO SW-TRAEFF                                 
054900       END-IF                                                             
055000     END-IF                                                               
055100                                                                          
055200*    IF  WS-PRISRAD  > SPACE                                              
055300*    OR  WS-PBRAD    > SPACE                                              
055400*    OR  WS-IDREFTAB > SPACE                                              
055500*      MOVE WS-PRISRAD       TO WS-PRISRAD-RED-X                          
055600*      MOVE WS-PBRAD         TO WS-PBRAD-RED-X                            
055700*      MOVE WS-IDREFTAB      TO WS-IDREFTAB-NUM                           
055800*      IF  WS-KLASS-X      = IN-KLASS                                     
055900*      AND WS-IDREFTAB-NUM = IN-IDREFTAB                                  
056000*        CONTINUE                                                         
056100*      ELSE                                                               
056200*        MOVE NEJ            TO SW-TRAEFF                                 
056300*      END-IF                                                             
056400*    END-IF                                                               
056500     IF  WS-IDREFTAB > SPACE                                              
056600       MOVE WS-IDREFTAB      TO WS-IDREFTAB-NUM                           
056700       IF  WS-IDREFTAB-NUM NOT = IN-IDREFTAB                              
056800         MOVE NEJ            TO SW-TRAEFF                                 
056900       END-IF                                                             
057000     END-IF                                                               
057100                                                                          
057200     MOVE IN-KLASS           TO WS-KLASS-IN                               
057300     IF  WS-PRISRAD > SPACE                                               
057400       MOVE WS-PRISRAD       TO WS-PRISRAD-NUM                            
057500       IF  WS-PRISRAD-NUM NOT = WS-PRISRAD-IN                             
057600         MOVE NEJ            TO SW-TRAEFF                                 
057700       END-IF                                                             
057800     END-IF                                                               
057900                                                                          
058000     IF WS-PBRAD > SPACE                                                  
058100       IF WS-PBRAD NOT = WS-PBRAD-IN                                      
058200         MOVE NEJ            TO SW-TRAEFF                                 
058300       END-IF                                                             
058400     END-IF                                                               
058500                                                                          
058600     IF WS-FLREFBEO > SPACE                                               
058700       IF   WS-FLREFBEO = IN-FLREFBEO                                     
058800       OR ((WS-FLREFBEO = JA                                              
058900       OR   WS-FLREFBEO = YES)                                            
059000       AND (IN-FLREFBEO = JA                                              
059100       OR   IN-FLREFBEO = YES))                                           
059200         CONTINUE                                                         
059300       ELSE                                                               
059400         MOVE NEJ            TO SW-TRAEFF                                 
059500       END-IF                                                             
059600     END-IF                                                               
059700                                                                          
059800     IF  WS-VKART > SPACE                                                 
059900       MOVE WS-VKART         TO WS-VKART-NUM                              
060000       IF  WS-VKART-TKN = '='                                             
060100         IF IN-VKART = WS-VKART-NUM                                       
060200           CONTINUE                                                       
060300         ELSE                                                             
060400           MOVE NEJ          TO SW-TRAEFF                                 
060500         END-IF                                                           
060600       END-IF                                                             
060700       IF  WS-VKART-TKN = '>'                                             
060800         IF IN-VKART > WS-VKART-NUM                                       
060900           CONTINUE                                                       
061000         ELSE                                                             
061100           MOVE NEJ          TO SW-TRAEFF                                 
061200         END-IF                                                           
061300       END-IF                                                             
061400       IF  WS-VKART-TKN = '<'                                             
061500         IF IN-VKART < WS-VKART-NUM                                       
061600           CONTINUE                                                       
061700         ELSE                                                             
061800           MOVE NEJ          TO SW-TRAEFF                                 
061900         END-IF                                                           
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     IF  WS-KVPB-REF-FOM > SPACE                                          
062400     AND WS-KVPB-REF-TOM > SPACE                                          
062500       MOVE WS-KVPB-REF-FOM  TO DEC-IDFRIDATA                             
062600       MOVE 6                TO DEC-KVHELTAL                              
062700       MOVE 1                TO DEC-KVDECIMAL                             
062800       CALL WDECEDIT USING DEC-WDECAREA                                   
062900       IF DEC-KDSVAR-OK                                                   
063000          MOVE DEC-IDEDITDATA                                             
063100                             TO WS-KVPB-REF-FOM-NUM                       
063200       ELSE                                                               
063300          MOVE ZERO          TO WS-KVPB-REF-FOM-NUM                       
063400       END-IF                                                             
063500       MOVE WS-KVPB-REF-TOM  TO DEC-IDFRIDATA                             
063600       MOVE 6                TO DEC-KVHELTAL                              
063700       MOVE 1                TO DEC-KVDECIMAL                             
063800       CALL WDECEDIT USING DEC-WDECAREA                                   
063900       IF DEC-KDSVAR-OK                                                   
064000          MOVE DEC-IDEDITDATA                                             
064100                             TO WS-KVPB-REF-TOM-NUM                       
064200       ELSE                                                               
064300          MOVE ZERO          TO WS-KVPB-REF-TOM-NUM                       
064400       END-IF                                                             
064500       IF  IN-KVPB-REF >= WS-KVPB-REF-FOM-NUM                             
064600       AND IN-KVPB-REF <= WS-KVPB-REF-TOM-NUM                             
064700         CONTINUE                                                         
064800       ELSE                                                               
064900         MOVE NEJ            TO SW-TRAEFF                                 
065000       END-IF                                                             
065100     END-IF                                                               
065200                                                                          
065300     IF  WS-VLARTNTO > SPACE                                              
065400       MOVE WS-VLARTNTO      TO WS-VLARTNTO-NUM                           
065500       IF  WS-VLARTNTO-TKN = '='                                          
065600         IF IN-VLARTNTO = WS-VLARTNTO-NUM                                 
065700           CONTINUE                                                       
065800         ELSE                                                             
065900           MOVE NEJ          TO SW-TRAEFF                                 
066000         END-IF                                                           
066100       END-IF                                                             
066200       IF  WS-VLARTNTO-TKN = '>'                                          
066300         IF IN-VLARTNTO > WS-VLARTNTO-NUM                                 
066400           CONTINUE                                                       
066500         ELSE                                                             
066600           MOVE NEJ          TO SW-TRAEFF                                 
066700         END-IF                                                           
066800       END-IF                                                             
066900       IF  WS-VLARTNTO-TKN = '<'                                          
067000         IF IN-VLARTNTO < WS-VLARTNTO-NUM                                 
067100           CONTINUE                                                       
067200         ELSE                                                             
067300           MOVE NEJ          TO SW-TRAEFF                                 
067400         END-IF                                                           
067500       END-IF                                                             
067600     END-IF                                                               
067700                                                                          
067800     IF  WS-ADLAGOMR > SPACE                                              
067900       MOVE WS-ADLAGOMR      TO WS-ADLAGOMR-NUM                           
068000       IF  WS-ADLAGOMR-NUM NOT = IN-ADLAGOMR                              
068100         MOVE NEJ            TO SW-TRAEFF                                 
068200       END-IF                                                             
068300     END-IF                                                               
068400                                                                          
068500     IF  WS-ADGANG > SPACE                                                
068600       MOVE WS-ADGANG        TO WS-ADGANG-NUM                             
068700       IF  WS-ADGANG-NUM NOT = IN-ADGANG                                  
068800         MOVE NEJ            TO SW-TRAEFF                                 
068900       END-IF                                                             
069000     END-IF                                                               
069100                                                                          
069200     IF  WS-ADPLATS-FOM > SPACE                                           
069300     AND WS-ADPLATS-TOM > SPACE                                           
069400       MOVE WS-ADPLATS-FOM   TO WS-ADPLATS-FOM-NUM                        
069500       MOVE WS-ADPLATS-TOM   TO WS-ADPLATS-TOM-NUM                        
069600       IF  IN-ADPLATS >= WS-ADPLATS-FOM-NUM                               
069700       AND IN-ADPLATS <= WS-ADPLATS-TOM-NUM                               
069800         CONTINUE                                                         
069900       ELSE                                                               
070000         MOVE NEJ            TO SW-TRAEFF                                 
070100       END-IF                                                             
070200     END-IF                                                               
070300                                                                          
070400     IF  WS-KDERS > SPACE                                                 
070500       MOVE WS-KDERS         TO WS-KDERS-NUM                              
070600       IF  WS-KDERS-NUM NOT = IN-KDERS                                    
070700         MOVE NEJ            TO SW-TRAEFF                                 
070800       END-IF                                                             
070900     END-IF                                                               
071000                                                                          
071100     IF  WS-PRARTSTD > SPACE                                              
071200       MOVE WS-PRARTSTD      TO WS-PRARTSTD-NUM                           
071300       IF  WS-PRARTSTD-TKN = '='                                          
071400         IF IN-PRARTSTD = WS-PRARTSTD-NUM                                 
071500           CONTINUE                                                       
071600         ELSE                                                             
071700           MOVE NEJ          TO SW-TRAEFF                                 
071800         END-IF                                                           
071900       END-IF                                                             
072000       IF  WS-PRARTSTD-TKN = '>'                                          
072100         IF IN-PRARTSTD > WS-PRARTSTD-NUM                                 
072200           CONTINUE                                                       
072300         ELSE                                                             
072400           MOVE NEJ          TO SW-TRAEFF                                 
072500         END-IF                                                           
072600       END-IF                                                             
072700       IF  WS-PRARTSTD-TKN = '<'                                          
072800         IF IN-PRARTSTD < WS-PRARTSTD-NUM                                 
072900           CONTINUE                                                       
073000         ELSE                                                             
073100           MOVE NEJ          TO SW-TRAEFF                                 
073200         END-IF                                                           
073300       END-IF                                                             
073400     END-IF                                                               
073500                                                                          
073600     IF  WS-KVLS > SPACE                                                  
073700       MOVE WS-KVLS          TO WS-KVLS-NUM                               
073800       IF  WS-KVLS-TKN = '='                                              
073900         IF IN-KVLS = WS-KVLS-NUM                                         
074000           CONTINUE                                                       
074100         ELSE                                                             
074200           MOVE NEJ          TO SW-TRAEFF                                 
074300         END-IF                                                           
074400       END-IF                                                             
074500       IF  WS-KVLS-TKN = '>'                                              
074600         IF IN-KVLS > WS-KVLS-NUM                                         
074700           CONTINUE                                                       
074800         ELSE                                                             
074900           MOVE NEJ          TO SW-TRAEFF                                 
075000         END-IF                                                           
075100       END-IF                                                             
075200       IF  WS-KVLS-TKN = '<'                                              
075300         IF IN-KVLS < WS-KVLS-NUM                                         
075400           CONTINUE                                                       
075500         ELSE                                                             
075600           MOVE NEJ          TO SW-TRAEFF                                 
075700         END-IF                                                           
075800       END-IF                                                             
075900     END-IF                                                               
076000                                                                          
076100     IF  WS-TIFINLV > SPACE                                               
076200       MOVE IN-TIFINLV       TO TMP1-YYWWD                                
076300       MOVE WS-TIFINLV       TO WS-TIFINLV-NUM                            
076400       MOVE WS-TIFINLV-NUM TO   TMP2-YYWWD                                
076500       PERFORM WY2000P2                                                   
076600       IF  WS-TIFINLV-TKN = '='                                           
076700         IF IN-TIFINLV = WS-TIFINLV-NUM                                   
076800           CONTINUE                                                       
076900         ELSE                                                             
077000           MOVE NEJ          TO SW-TRAEFF                                 
077100         END-IF                                                           
077200       END-IF                                                             
077300       IF  WS-TIFINLV-TKN = '>'                                           
077400         IF TMP1-YYWWD > TMP2-YYWWD                                       
077500           CONTINUE                                                       
077600         ELSE                                                             
077700           MOVE NEJ          TO SW-TRAEFF                                 
077800         END-IF                                                           
077900       END-IF                                                             
078000       IF  WS-TIFINLV-TKN = '<'                                           
078100         IF TMP1-YYWWD < TMP2-YYWWD                                       
078200           CONTINUE                                                       
078300         ELSE                                                             
078400           MOVE NEJ          TO SW-TRAEFF                                 
078500         END-IF                                                           
078600       END-IF                                                             
078700     END-IF                                                               
078800                                                                          
078900     IF  WS-TIREFEFT > SPACE                                              
079000       MOVE WS-TIREFEFT      TO WS-TIREFEFT-NUM                           
079100       IF  WS-TIREFEFT-TKN = '='                                          
079200         IF IN-TIREFEFT = WS-TIREFEFT-NUM                                 
079300           CONTINUE                                                       
079400         ELSE                                                             
079500           MOVE NEJ          TO SW-TRAEFF                                 
079600         END-IF                                                           
079700       END-IF                                                             
079800       IF  WS-TIREFEFT-TKN = '>'                                          
079900         MOVE IN-TIREFEFT  TO TMP1-YYMMDD                                 
080000         MOVE WS-TIREFEFT-NUM                                             
080100                           TO TMP2-YYMMDD                                 
080200         PERFORM WY2000P1                                                 
080300         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
080400           CONTINUE                                                       
080500         ELSE                                                             
080600           MOVE NEJ          TO SW-TRAEFF                                 
080700         END-IF                                                           
080800       END-IF                                                             
080900       IF  WS-TIREFEFT-TKN = '<'                                          
081000         MOVE IN-TIREFEFT  TO TMP1-YYMMDD                                 
081100         MOVE WS-TIREFEFT-NUM                                             
081200                           TO TMP2-YYMMDD                                 
081300         PERFORM WY2000P1                                                 
081400         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
081500           CONTINUE                                                       
081600         ELSE                                                             
081700           MOVE NEJ          TO SW-TRAEFF                                 
081800         END-IF                                                           
081900       END-IF                                                             
082000     END-IF                                                               
082100                                                                          
082200     IF  WS-BEART > SPACE                                                 
082300*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
082400       MOVE ZERO             TO FLENG                                     
082500       INSPECT FUNCTION REVERSE (WS-BEART)                                
082600         TALLYING FLENG FOR LEADING SPACE                                 
082700       COMPUTE FLENG = 25 - FLENG                                         
082800                                                                          
082900       IF WS-IDLAND-SPR = 'GB'                                            
083000         MOVE 1              TO IX-FROM                                   
083100         PERFORM UNTIL (IX-FROM + FLENG + 1) > 25                         
083200         OR WS-BEART (1:FLENG) = IN-BEART-ENG (IX-FROM:FLENG)             
083300           ADD 1             TO IX-FROM                                   
083400         END-PERFORM                                                      
083500       ELSE                                                               
083600         MOVE 1              TO IX-FROM                                   
083700         PERFORM UNTIL (IX-FROM + FLENG + 1) > 25                         
083800         OR WS-BEART (1:FLENG) = IN-BEART (IX-FROM:FLENG)                 
083900           ADD 1             TO IX-FROM                                   
084000         END-PERFORM                                                      
084100       END-IF                                                             
084200                                                                          
084300       IF (IX-FROM + FLENG + 1) > 25                                      
084400         MOVE NEJ            TO SW-TRAEFF                                 
084500       END-IF                                                             
084600                                                                          
084700     END-IF                                                               
084800                                                                          
084900     IF  WS-BEMODELL > SPACE                                              
085000*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
085100       MOVE ZERO             TO FLENG                                     
085200       INSPECT FUNCTION REVERSE (WS-BEMODELL)                             
085300         TALLYING FLENG FOR LEADING SPACE                                 
085400       COMPUTE FLENG = 15 - FLENG                                         
085500                                                                          
085600       MOVE 1                TO IX-BEMOD                                  
085700       MOVE NEJ TO BEMODELL-TRAEFF-SW                                     
085800       PERFORM UNTIL IX-BEMOD > 45                                        
085900       OR IN-BEMODELL(IX-BEMOD) = SPACE                                   
086000       OR BEMODELL-TRAEFF-SW = JA                                         
086100         MOVE IN-BEMODELL(IX-BEMOD) TO W-BEMODELL                         
086200         MOVE 1              TO IX-FROM                                   
086300         PERFORM UNTIL (IX-FROM + FLENG + 1) > 15                         
086400         OR WS-BEMODELL (1:FLENG) = W-BEMODELL (IX-FROM:FLENG)            
086500           ADD 1             TO IX-FROM                                   
086600         END-PERFORM                                                      
086700                                                                          
086800         IF (IX-FROM + FLENG + 1) < 15                                    
086900           MOVE JA           TO BEMODELL-TRAEFF-SW                        
087000*        CALL FELLOG                                                      
087100         END-IF                                                           
087200         ADD 1              TO IX-BEMOD                                   
087300       END-PERFORM                                                        
087400       IF BEMODELL-TRAEFF-SW = NEJ                                        
087500         MOVE NEJ          TO SW-TRAEFF                                   
087600       END-IF                                                             
087700     END-IF                                                               
087800                                                                          
087900     IF WS-KDREFSTA > SPACE                                               
088000       IF   WS-KDREFSTA = IN-KDREFSTA                                     
088100*      OR ((WS-KDREFSTA = 'A'                                             
088200*      OR   WS-KDREFSTA = 'P')                                            
088300*      AND (IN-KDREFSTA = 'A'           GAMMAL                            
088400*      OR   IN-KDREFSTA = 'P'))                                           
088500******************************************                                
088600*      OR ((WS-KDREFSTA = 'A'                                             
088700*      AND  IN-KDREFSTA = 'A')          NY                                
088800*      OR  (WS-KDREFSTA = 'P'                                             
088900*      AND  IN-KDREFSTA = 'P'))                                           
089000         CONTINUE                                                         
089100       ELSE                                                               
089200         MOVE NEJ            TO SW-TRAEFF                                 
089300       END-IF                                                             
089400     END-IF                                                               
089500                                                                          
089600     IF  WS-SUPERWEEK > SPACE                                             
089700       MOVE WS-SUPERWEEK     TO WS-SUPERWEEK-NUM                          
089800       IF  WS-SUPERWEEK-TKN = '='                                         
089900         IF IN-SUPERWEEK = WS-SUPERWEEK-NUM                               
090000           CONTINUE                                                       
090100         ELSE                                                             
090200           MOVE NEJ          TO SW-TRAEFF                                 
090300         END-IF                                                           
090400       END-IF                                                             
090500       IF  WS-SUPERWEEK-TKN = '>'                                         
090600         MOVE IN-SUPERWEEK TO TMP1-YYMMDD                                 
090700         MOVE WS-SUPERWEEK-NUM                                            
090800                           TO TMP2-YYMMDD                                 
090900         PERFORM WY2000P1                                                 
091000         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
091100           CONTINUE                                                       
091200         ELSE                                                             
091300           MOVE NEJ          TO SW-TRAEFF                                 
091400         END-IF                                                           
091500       END-IF                                                             
091600       IF  WS-SUPERWEEK-TKN = '<'                                         
091700         MOVE IN-SUPERWEEK TO TMP1-YYMMDD                                 
091800         MOVE WS-SUPERWEEK-NUM                                            
091900                           TO TMP2-YYMMDD                                 
092000         PERFORM WY2000P1                                                 
092100         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
092200           CONTINUE                                                       
092300         ELSE                                                             
092400           MOVE NEJ          TO SW-TRAEFF                                 
092500         END-IF                                                           
092600       END-IF                                                             
092700     END-IF                                                               
092800                                                                          
092900     IF WS-FLFLYG > SPACE                                                 
093000       IF   WS-FLFLYG = IN-FLFLYG                                         
093100       OR ((WS-FLFLYG = JA                                                
093200       OR   WS-FLFLYG = YES)                                              
093300       AND (IN-FLFLYG = JA                                                
093400       OR   IN-FLFLYG = YES))                                             
093500         CONTINUE                                                         
093600       ELSE                                                               
093700         MOVE NEJ            TO SW-TRAEFF                                 
093800       END-IF                                                             
093900     END-IF                                                               
094000                                                                          
094100     .                                                                    
094200     EJECT                                                                
094300 C-BEHANDLA-UTPOST SECTION.                                               
094400                                                                          
094500     MOVE SPACE              TO UT-AREA                                   
094600     MOVE '002'              TO UT-IDPTYP                                 
094700     MOVE IN-IDARTNR         TO UT-IDARTNR-RES                            
094800     MOVE IN-IDDC            TO UT-IDDC-RES                               
094900     IF WS-IDLAND-SPR = 'GB'                                              
095000       MOVE IN-BEART-ENG     TO UT-BEART-RES                              
095100     ELSE                                                                 
095200       MOVE IN-BEART         TO UT-BEART-RES                              
095300     END-IF                                                               
095400     MOVE IN-KVLS            TO UT-KVLS-RES                               
095500     MOVE IN-ADLAGOMR        TO UT-ADLAGOMR-RES                           
095600     MOVE IN-ADGANG          TO UT-ADGANG-RES                             
095700     MOVE IN-ADPLATS         TO UT-ADPLATS-RES                            
095800     MOVE IN-KVPB-REF        TO UT-KVPB-REF-RES                           
095900     MOVE IN-PRARTSTD        TO UT-PRARTSTD-RES                           
096000                                                                          
096100     PERFORM S11-SKRIV-W27128                                             
096200     .                                                                    
096300     EJECT                                                                
096400 Z-FINIT SECTION.                                                         
096500                                                                          
096600     CLOSE W20348                                                         
096700           W27127                                                         
096800           W27128                                                         
096900     SKIP2                                                                
097000     MOVE 'S' TO POSTSUM-OPKOD                                            
097100     CALL POSTSUM USING POSTSUM-PARM                                      
097200     .                                                                    
097300     EJECT                                                                
097400 S01-READ-W27127  SECTION.                                                
097500     SKIP2                                                                
097600     READ W27127 INTO IN-AREA                                             
097700     AT END                                                               
097800        SET END-OF-W27127 TO TRUE                                         
097900                                                                          
098000     NOT AT END                                                           
098100        MOVE 'W27127' TO POSTSUM-FDNAMN                                   
098200        MOVE 'W27128D1' TO POSTSUM-DDNAMN2                                
098300        CALL POSTSUM USING POSTSUM-PARM                                   
098400     END-READ                                                             
098500     .                                                                    
098600     EJECT                                                                
098700 S02-LAES-W20348  SECTION.                                                
098800                                                                          
098900     READ W20348                                                          
099000     AT END                                                               
099100        SET END-OF-W20348 TO TRUE                                         
099200                                                                          
099300     NOT AT END                                                           
099400        MOVE 'W27128' TO POSTSUM-FDNAMN                                   
099500        MOVE 'W27128D1' TO POSTSUM-DDNAMN2                                
099600        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
099700        CALL POSTSUM USING POSTSUM-PARM                                   
099800     END-READ                                                             
099900     .                                                                    
100000     EJECT                                                                
100100 S11-SKRIV-W27128 SECTION.                                                
100200     SKIP2                                                                
100300     WRITE UT-POST FROM UT-AREA                                           
100400                                                                          
100500     MOVE 'W27128 '  TO POSTSUM-FDNAMN                                    
100600     MOVE 'W27128D2' TO POSTSUM-DDNAMN2                                   
100700     CALL POSTSUM    USING POSTSUM-PARM                                   
100800     .                                                                    
100900     EJECT                                                                
101000                                                                          
101100 S99-ABEND SECTION.                                                       
101200                                                                          
101300     SKIP2                                                                
101400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
101500     .                                                                    
101600     EJECT                                                                
101700*    -COPY WY2000P1                                                       
101800     EJECT                                                                
101900*    -COPY WY2000P2                                                       
