000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4639800.                                                
000301 AUTHOR.         KJELLSON GÖRAN.                                          
000401 DATE-WRITTEN.   14/02/27.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        REDIGERA LISTA FÖR EJ SKICKADE DDGS-ORDER                        
000901*        FÖR VARJE LEVERANTÖR SKICKAS DATA TILL D&P, VIA DAP3,            
001000*                                                                         
001200*                                                                         
001301* INFIL W4639M - UPPFÖLJNINGSFIL EJ SKICKADE DDGS-ORDER                   
001401*                                                                         
001501* URFIL W4639N - DDGS-POSTER TILL D&P                                     
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000                                                                          
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700                                                                          
002801*          --- EJ SKICKADE DDGS-ORDER                                     
002901     SELECT W4639M                     ASSIGN TO W46398D1.                
003001*          --- DDGS-POSTER TILL D&P                                       
003102     SELECT w4639N                     ASSIGN TO W46398D2.                
003200                                                                          
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500                                                                          
003601 FD  W4639M                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003901*01  POST      -COPY W4639M -PRE  DDGS-  -L.                              
004000                                                                          
004101 FD  W4639N                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400 01  DOP-POST  PIC X(100).                                                
004500                                                                          
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004901 77  IDPGM                       PIC X(8)    VALUE 'W4639800'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005300 77  CURRENT-IDLEVNR             PIC X(5)    VALUE SPACE.                 
005500                                                                          
005601 77  W4639M-EOF-SW               PIC X       VALUE 'N'.                   
005701     88  EOF-W4639M                          VALUE 'J'.                   
005800                                                                          
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400                                                                          
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600                                                                          
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100                                                                          
007200*    --- PARAMETRAR TILL ABEND                                            
007300                                                                          
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007700                                                                          
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500                                                                          
008601 01  DDGS-AREA-START             PIC X(24)   VALUE                        
008701                                 'DDGS-AREA-START  '.                     
008800                                                                          
008901*01  AREA -COPY W4639M     -PRE DDGS-                                     
009000                                                                          
009100 01  BLANK-RAD.                                                           
009200     03  FILLER            PIC X(01) VALUE SPACE.                         
009300     03  FILLER  PIC X     VALUE X'5E'.                                   
009400                                                                          
009501 01  RUBRIK-1.                                                            
009601     03  FILLER            PIC X(16) VALUE                                
009602        ' HEADS UP REPORT'.                                               
009603                                                                          
009604 01  RUBRIK-2.                                                            
009605     03  FILLER            PIC X(36) VALUE                                
009701        ' DDGS-Order to be sent to Supplier: '.                           
009803     03  RUB-IDLEVNR       PIC X(5) VALUE SPACE.                          
009900     03  FILLER  PIC X     VALUE X'5E'.                                   
010000                                                                          
010501 01  RUBRIK3.                                                             
010601     03  FILLER            PIC X(14) VALUE  ' Shipment date'.             
010700     03  FILLER            PIC X     VALUE X'5E'.                         
010801     03  FILLER            PIC X(22) VALUE                                
010901                          'Send order to supplier'.                       
011001     03  FILLER            PIC X     VALUE X'5E'.                         
011101     03  FILLER            PIC X(04) VALUE  'DIST'.                       
011201     03  FILLER            PIC X     VALUE X'5E'.                         
011301     03  FILLER            PIC X(04) VALUE  'CUST'.                       
011401     03  FILLER            PIC X     VALUE X'5E'.                         
011501     03  FILLER            PIC X(03) VALUE  'Ord'.                        
011601     03  FILLER            PIC X     VALUE X'5E'.                         
011701     03  FILLER            PIC X(04) VALUE  'PROD'.                       
011801     03  FILLER            PIC X     VALUE X'5E'.                         
011901     03  FILLER            PIC X(04) VALUE  'PART'.                       
012001     03  FILLER            PIC X     VALUE X'5E'.                         
012101     03  FILLER            PIC X(07) VALUE  'ORD.QTY'.                    
012201     03  FILLER            PIC X     VALUE X'5E'.                         
012301     03  FILLER            PIC X(02) VALUE  'CL'.                         
012401     03  FILLER            PIC X     VALUE X'5E'.                         
012501     03  FILLER            PIC X(08) VALUE  'LINE NO.'.                   
012601     03  FILLER            PIC X     VALUE X'5E'.                         
013101                                                                          
013201                                                                          
021501 01  DDGS-RAD.                                                            
021601     03  FILLER            PIC X    VALUE SPACE.                          
021701     03  RAD-DASKEPPN      PIC 9(8).                                      
021801     03  FILLER            PIC X    VALUE X'5E'.                          
021802     03  RAD-DASNDDAT      PIC 9(8).                                      
021803     03  FILLER            PIC X    VALUE X'5E'.                          
022301     03  RAD-IDDISTR       PIC Z(3)9.                                     
022401     03  FILLER            PIC X    VALUE X'5E'.                          
022501     03  RAD-IDKUNDNR      PIC Z(5)9.                                     
022601     03  FILLER            PIC X    VALUE X'5E'.                          
022701     03  RAD-IDORDNR7      PIC Z(6)9.                                     
022801     03  FILLER            PIC X    VALUE X'5E'.                          
022901     03  RAD-IDPRODNR      PIC Z(6)9.                                     
023001     03  FILLER            PIC X    VALUE X'5E'.                          
023002     03  RAD-IDARTNR       PIC Z(7)9.                                     
023003     03  FILLER            PIC X    VALUE X'5E'.                          
023101     03  RAD-KVBEART       PIC z(5)9.                                     
023201     03  FILLER            PIC X    VALUE X'5E'.                          
023301     03  RAD-KDORDKL       PIC 9.                                         
023401     03  FILLER            PIC X    VALUE X'5E'.                          
023501     03  RAD-IDPURAD       PIC Z(3)9.                                     
023601     03  FILLER            PIC X    VALUE X'5E'.                          
024101                                                                          
031701                                                                          
031801 01  W001-DAP.                                                            
031901     03  FILLER                  PIC X(165)  VALUE SPACE.                 
032001                                                                          
032101                                                                          
032201                                                                          
032301 PROCEDURE DIVISION.                                                      
032401 MAIN SECTION.                                                            
032501                                                                          
032601                                                                          
032701     PERFORM A-INIT                                                       
032801                                                                          
032901     PERFORM S01-READ-W4639M                                              
033001     PERFORM UNTIL EOF-W4639M                                             
033101                                                                          
033201        IF DDGS-IDLEVNR NOT = CURRENT-IDLEVNR                             
033301           PERFORM B-NY-LEVERANTOR                                        
033501        END-IF                                                            
033901        PERFORM C-NY-DDGSPOST                                             
034001                                                                          
034101        PERFORM S01-READ-W4639M                                           
034201     END-PERFORM                                                          
034301                                                                          
034401     PERFORM Z-FINIT                                                      
034501                                                                          
034601     MOVE ZERO TO RETURN-CODE                                             
034701     GOBACK                                                               
034801     .                                                                    
034901                                                                          
035001                                                                          
035101 A-INIT SECTION.                                                          
035201     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
035301                                                                          
035401     OPEN INPUT  W4639M                                                   
035501     OPEN OUTPUT w4639N                                                   
035601                                                                          
035701     ACCEPT DAGENS-DATUM  FROM DATE                                       
035801     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035901     .                                                                    
036001                                                                          
036101                                                                          
036201 B-NY-LEVERANTOR    SECTION.                                              
036301     MOVE 'B-NY-LEVERANTOR ' TO CURRENT-SECTION                           
036401                                                                          
036501     MOVE '¤DAPWAITING-DDGS' TO W001-DAP                                  
036601     WRITE DOP-POST FROM W001-DAP                                         
036701                                                                          
036801     MOVE SPACE              TO W001-DAP                                  
036901     STRING '¤DAP' DDGS-IDLEVNR                                           
037001     DELIMITED BY SIZE INTO W001-DAP                                      
037101     WRITE DOP-POST FROM W001-DAP                                         
037201                                                                          
037301     IF RUB-IDLEVNR NOT = SPACE                                           
037401        WRITE DOP-POST FROM BLANK-RAD                                     
037501        WRITE DOP-POST FROM BLANK-RAD                                     
037601     END-IF                                                               
037602     WRITE DOP-POST FROM RUBRIK-1                                         
037701     MOVE DDGS-IDLEVNR       TO RUB-IDLEVNR                               
037803     WRITE DOP-POST FROM RUBRIK-2                                         
037901     MOVE 'W4639M'       TO POSTSUM-FDNAMN                                
038001     MOVE 'W46398D2'     TO POSTSUM-DDNAMN2                               
038101     MOVE 'LEV'          TO POSTSUM-TRANSTYP                              
038201     CALL POSTSUM USING     POSTSUM-PARM                                  
038301     MOVE DDGS-IDLEVNR   TO CURRENT-IDLEVNR                               
038403                                                                          
038503     WRITE DOP-POST FROM BLANK-RAD                                        
038603     WRITE DOP-POST FROM RUBRIK3                                          
038705     .                                                                    
038801                                                                          
038901                                                                          
041701 C-NY-DDGSPOST      SECTION.                                              
041801     MOVE 'C-NY-DDGSPOST   ' TO CURRENT-SECTION                           
041901                                                                          
042004     MOVE DDGS-DASKEPPN     TO RAD-DASKEPPN                               
042203     MOVE DDGS-IDARTNR      TO RAD-IDARTNR                                
042404     MOVE DDGS-DASNDDAT     TO RAD-DASNDDAT                               
042603     MOVE DDGS-IDDISTR      TO RAD-IDDISTR                                
042804     MOVE DDGS-IDKUNDNR     TO RAD-IDKUNDNR                               
043004     MOVE DDGS-IDORDNR7     TO RAD-IDORDNR7                               
043204     MOVE DDGS-IDPRODNR     TO RAD-IDPRODNR                               
043403     MOVE DDGS-KVBEART      TO RAD-KVBEART                                
043603     MOVE DDGS-KDORDKL      TO RAD-KDORDKL                                
043803     MOVE DDGS-IDPURAD      TO RAD-IDPURAD                                
043901     WRITE DOP-POST FROM DDGS-RAD                                         
044001                                                                          
044102     MOVE 'W4639N'       TO POSTSUM-FDNAMN                                
044201     MOVE 'W46398D2'     TO POSTSUM-DDNAMN2                               
044301     MOVE DDGS-IDLEVNR   TO POSTSUM-TRANSTYP                              
044401     CALL POSTSUM USING     POSTSUM-PARM                                  
044501     .                                                                    
044601                                                                          
044701                                                                          
052101 Z-FINIT SECTION.                                                         
052201     CLOSE W4639M W4639N                                                  
052301                                                                          
052401     MOVE 'S'        TO POSTSUM-OPKOD                                     
052501     CALL POSTSUM USING POSTSUM-PARM                                      
052601     .                                                                    
052701                                                                          
052801                                                                          
052901 S01-READ-W4639M  SECTION.                                                
053001     MOVE 'S01-READ-W4639M ' TO CURRENT-SECTION                           
053101                                                                          
053201     READ W4639M INTO DDGS-AREA                                           
053301     AT END                                                               
053401        SET EOF-W4639M TO TRUE                                            
053501                                                                          
053601     NOT AT END                                                           
053701        MOVE 'W4639M'     TO POSTSUM-FDNAMN                               
053801        MOVE 'W46398D1'   TO POSTSUM-DDNAMN2                              
053901        MOVE DDGS-IDLEVNR TO POSTSUM-TRANSTYP                             
054001        CALL POSTSUM USING   POSTSUM-PARM                                 
054101     END-READ                                                             
055001     .                                                                    
