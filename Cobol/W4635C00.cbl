000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4635C00.                                                
000301 AUTHOR.         KJELLSON GÖRAN.                                          
000401 DATE-WRITTEN.   HÖSTEN  2019.                                            
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        FELAKTIG BRUTTOVIKT VIA D&P                                      
000901*                                                                         
001001* INFIL W4635D - JUSTERADE KOLLI (PACKTRANASAR)                           
001101*                                                                         
001201* UTFIL W4635E - JUSTERADE KOLLI  TILL D&P                                
001301*                                                                         
001701*                                                                         
001801*    ABENDKODER:                                                          
001901*        U0016 -  . . . .                                                 
002001*        U1000 -  . . . .                                                 
002101*                                                                         
002201                                                                          
002301                                                                          
002401 ENVIRONMENT DIVISION.                                                    
002501 INPUT-OUTPUT SECTION.                                                    
002601                                                                          
002701 FILE-CONTROL.                                                            
002801                                                                          
002901*          --- FELTRANSAR                                                 
003001     SELECT W4635D                     ASSIGN TO W4635CD1.                
003101                                                                          
003201*          --- FELTRANSAR TILL D&P                                        
003301     SELECT W4635E                     ASSIGN TO W4635CD2.                
003401                                                                          
003501                                                                          
003601 DATA DIVISION.                                                           
003701 FILE SECTION.                                                            
003801                                                                          
003901 FD  W4635D                                                               
004001     RECORDING       F                                                    
004101     BLOCK CONTAINS  0.                                                   
004201                                                                          
004301*01  -COPY W4635J      -L.                                                
004501                                                                          
004601                                                                          
004701 FD  W4635E                                                               
004801     RECORDING       V                                                    
004901     BLOCK CONTAINS  0.                                                   
005002 01  DOP-POST  PIC X(115).                                                
005101                                                                          
005201 WORKING-STORAGE SECTION.                                                 
005301                                                                          
005401 77  IDPGM                       PIC X(8)    VALUE 'W4635900'.            
005501 77  JA                          PIC X       VALUE 'J'.                   
005601 77  NEJ                         PIC X       VALUE 'N'.                   
005701                                                                          
005800 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005901                                                                          
006000 77  KDRC-DISPLAY                PIC Z(3)9.                               
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200                                                                          
006302 77  W4635d-EOF-SW               PIC X       VALUE 'N'.                   
006402     88  END-OF-W4635d                       VALUE 'J'.                   
006500                                                                          
006600                                                                          
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007201 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
007300                                                                          
007400                                                                          
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600                                                                          
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008100                                                                          
008200                                                                          
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800                                                                          
008900 01  HJAELP-FAELT.                                                        
009000     03  CURR-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
009100     03  CURR-IDORDNR7           PIC 9(7)    VALUE ZERO.                  
009101     03  CURR-IDKOLLI            PIC 9(5)    VALUE ZERO.                  
009102     03  CURR-IDSUPREF           PIC X(10)   VALUE SPACE.                 
009201     03  CURR-DAREGDAT           PIC 9(8)    VALUE ZERO.                  
009202     03  CURR-VKART-SUM          PIC 9(8)V9  VALUE ZERO.                  
009401                                                                          
009402     03  CURR-VKART              PIC 9(8)V9  VALUE ZERO.                  
009403                                                                          
011100 01  MAIL-AREA.                                                           
011200     03  MAIL-BLANKRAD           PIC X       VALUE SPACE.                 
011300                                                                          
011401     03  MAIL-INTRO-1.                                                    
011501         05  FILLER              PIC X(05)   VALUE                        
011601            'Hello'.                                                      
011701     03  MAIL-INTRO-2.                                                    
011801         05  FILLER              PIC X(43)   VALUE                        
011901            'BP2T7 has received an EDI transaction from '.                
012001         05  INTRO-IDLEVNR       PIC X(05)   VALUE SPACE.                 
012101         05  FILLER              PIC X(30)   VALUE                        
012202            ' that is adjusted in the PULS.'.                             
012301     03  MAIL-INTRO-3.                                                    
012401         05  FILLER              PIC X(25)   VALUE                        
012501            'The message was received '.                                  
012601         05  INTRO-DATE          PIC X(06)   VALUE SPACE.                 
012701         05  FILLER              PIC X(01)   VALUE SPACE.                 
012801         05  INTRO-TIME          PIC X(05)   VALUE SPACE.                 
012902     03  MAIL-INTRO-3A.                                                   
013002         05  FILLER              PIC X(51)   VALUE                        
013102            'PULS has received a Gross weight from the supplier '.        
013202         05  FILLER              PIC X(50)   VALUE                        
013302            'that is lower than the sum of weight of the parts.'.         
013402     03  MAIL-INTRO-3B.                                                   
013502         05  FILLER              PIC X(50)   VALUE                        
013602            'Due to that we do not know if the part net weight '.         
013702         05  FILLER              PIC X(47)   VALUE                        
013802            'in PULS is wrong, or the gross weight from the '.            
013902         05  FILLER              PIC X(18)   VALUE                        
014002            'supplier is wrong,'.                                         
014102     03  MAIL-INTRO-3C.                                                   
014202         05  FILLER              PIC X(31)   VALUE                        
014302            'the parts needs to be analysed.'.                            
014402     03  MAIL-INTRO-3D.                                                   
014502         05  FILLER              PIC X(45)   VALUE                        
014602            'We have meantime adjusted the supplier gross '.              
014702         05  FILLER              PIC X(48)   VALUE                        
014802            'weight so we can send the goods to the customer.'.           
014902     03  MAIL-INTRO-3E.                                                   
015002         05  FILLER              PIC X(50)   VALUE                        
015102            'Please investigate if the net weight of the parts '.         
015202         05  FILLER              PIC X(48)   VALUE                        
015302            'or the supplier gross weight was wrong to avoid '.           
015402         05  FILLER              PIC X(14)   VALUE                        
015502            'future errors.'.                                             
015602     03  MAIL-INTRO-4.                                                    
015702         05  FILLER              PIC X(13)   VALUE                        
015802            'Message type:'.                                              
015902     03  MAIL-RUBRIK-1.                                                   
016002         05  FILLER              PIC X(25)   VALUE                        
016102            ' Adjusted Despatch Advice'.                                  
016202         05  FILLER              PIC X(20)   VALUE SPACE.                 
016302         05  FILLER              PIC X(19)   VALUE                        
016402            'Productionnumber.: '.                                        
016502         05  MAIL-IDPRODNR       PIC Z(6)9   VALUE ZERO.                  
016602     03  MAIL-RUBRIK-2.                                                   
016702         05  FILLER              PIC X(30)   VALUE                        
016802            ' Reported Gross weight in kg: '.                             
017202         05  MAIL-VKORDBTO       PIC Z(6).9  VALUE ZERO.                  
017302     03  MAIL-RUBRIK-3.                                                   
017402         05  FILLER              PIC X(32)   VALUE                        
017502            ' Gross weight rounded up to kg: '.                           
017602         05  MAIL-VKORDBTO-ADJ   PIC Z(6).9  VALUE ZERO.                  
017702                                                                          
017802     03  MAIL-REFERENS.                                                   
017902         05  FILLER              PIC X(19)   VALUE                        
018002            ' Reference......: '.                                         
018102         05  MAIL-IDSUPREF       PIC X(10)   VALUE SPACE.                 
018202                                                                          
018302     03  MAIL-DATUM.                                                      
018402         05  FILLER              PIC X(22)   VALUE                        
018502            ' Date...............: '.                                     
018602         05  MAIL-DAREGDAT       PIC 9(8)    VALUE ZERO.                  
018702                                                                          
018802     03  MAIL-FELORSAK.                                                   
018902         05  FILLER              PIC X(19)   VALUE                        
019002            ' Error Reasoncode: '.                                        
019102         05  MAIL-FELTEXT        PIC X(33)   VALUE                        
019202            'Gross weight less than net weight'.                          
019302                                                                          
019402                                                                          
019502     03  MAIL-ORDER.                                                      
019602         05  FILLER              PIC X(11)   VALUE                        
019702            ' Supplier: '.                                                
019802         05  MAIL-IDLEVNR        PIC X(5)    VALUE SPACE.                 
019902         05  FILLER              PIC X(12)   VALUE                        
020002            '  District: '.                                               
020102         05  MAIL-IDDISTR        PIC Z(3)9   VALUE ZERO.                  
020202         05  FILLER              PIC X(8)    VALUE                        
020302            '  Cust: '.                                                   
020402         05  MAIL-IDKUNDNR       PIC Z(5)9   VALUE ZERO.                  
020502         05  FILLER              PIC X(12)   VALUE                        
020602            '  Order no: '.                                               
020702         05  MAIL-IDORDNR        PIC Z(4)9   VALUE ZERO.                  
020802         05  FILLER              PIC X(6)    VALUE                        
020902            '  DC: '.                                                     
021002         05  MAIL-IDDC           PIC X(2)    VALUE SPACE.                 
021102                                                                          
021202                                                                          
021302                                                                          
021402     03  MAIL-KOLLI.                                                      
021502         05  FILLER              PIC X(7)    VALUE                        
021602            ' Case: '.                                                    
021702         05  MAIL-IDKOLLI        PIC X(5)    VALUE SPACE.                 
021802                                                                          
021902                                                                          
022002                                                                          
022102     03  MAIL-RADRUBRIK.                                                  
022202         05  FILLER              PIC X(53)   VALUE                        
022402          ' Line no  Part        Qty Calculated net weight in kg'.        
022502                                                                          
022602     03  MAIL-RAD.                                                        
022702         05  FILLER              PIC X(1)    VALUE SPACE.                 
022802         05  MAIL-IDRADNR        PIC Z(4)    VALUE ZERO.                  
022902         05  FILLER              PIC X(5)    VALUE SPACE.                 
023002         05  MAIL-IDARTNR        PIC Z(8)    VALUE ZERO.                  
023102         05  FILLER              PIC X(1)    VALUE SPACE.                 
023202         05  MAIL-KVLEVART       PIC Z(6)    VALUE ZERO.                  
023302         05  FILLER              PIC X(1)    VALUE SPACE.                 
023402         05  MAIL-VKART          PIC Z(6).9  VALUE ZERO.                  
023502                                                                          
023503     03  MAIL-SUM-RAD.                                                    
023504         05  FILLER              PIC X(24)   VALUE                        
023505            ' Total case net weight: '.                                   
023520         05  MAIL-SUM            PIC Z(6).9  VALUE ZERO.                  
023530                                                                          
023602                                                                          
023702                                                                          
024402     03  MAIL-SEPARATOR.                                                  
024502         05  FILLER              PIC X(40)   VALUE                        
024602            '----------------------------------------'.                   
024702         05  FILLER              PIC X(40)   VALUE                        
024802            '----------------------------------------'.                   
024902                                                                          
028802                                                                          
029702     03  MAIL-END-1.                                                      
029802         05  FILLER              PIC X(46)   VALUE                        
029902           'If you have any questions about this message, '.              
030002         05  FILLER              PIC X(21)   VALUE                        
030102           'please send a mail to'.                                       
030202     03  MAIL-END-2.                                                      
030302         05  FILLER              PIC X(22)   VALUE                        
030402           'DCIDHELP@VOLVOCARS.COM'.                                      
030502                                                                          
030602 01  W001-DAP.                                                            
030702     03  FILLER                  PIC X(165)  VALUE SPACE.                 
030802*    --- PARAMETRAR TILL POSTSUM                                          
030902                                                                          
031002*01  -COPY W0005   -PRE  POSTSUM-                                         
031102                                                                          
031202                                                                          
031302 01  W4635D-AREA-START           PIC X(24)   VALUE                        
031402                                 'W4635D-AREA-START  '.                   
031502                                                                          
031602 01  W4635D-AREA.                                                         
031902*   03  FILLER -COPY W4635J  -PRE RAD-                                    
032102                                                                          
032202                                                                          
043002                                                                          
043102                                                                          
043202 PROCEDURE DIVISION.                                                      
043702                                                                          
043802     PERFORM A-INIT                                                       
043902     PERFORM S01-LAES-W4635D                                              
044002                                                                          
044102     PERFORM UNTIL END-OF-W4635D                                          
044202                                                                          
044203        IF RAD-IDPRODNR NOT = CURR-IDPRODNR                               
044302           PERFORM B-NYTT-IDPRODNR                                        
044303           PERFORM C-SKRIV-HEADER                                         
044304        END-IF                                                            
044402                                                                          
044403        IF RAD-IDORDNR7 NOT = CURR-IDORDNR7                               
044404        OR RAD-IDPRODNR = ZERO                                            
044702           PERFORM D-NY-ORDER                                             
044703        END-IF                                                            
044802                                                                          
044803        IF RAD-IDKOLLI NOT = CURR-IDKOLLI                                 
044804        OR RAD-IDKOLLI = ZERO                                             
044805              PERFORM E-NYTT-KOLLI                                        
044806        END-IF                                                            
044807                                                                          
044808        PERFORM F-NY-RAD                                                  
044809        PERFORM S01-LAES-W4635D                                           
044810                                                                          
044820        IF END-OF-W4635D                                                  
044830        OR RAD-IDPRODNR NOT = CURR-IDPRODNR                               
044850           IF RAD-IDPRODNR NOT = CURR-IDPRODNR                            
044860           OR RAD-IDKOLLI NOT = CURR-IDKOLLI                              
044870              MOVE CURR-VKART-SUM TO MAIL-SUM                             
044880              WRITE DOP-POST FROM MAIL-SUM-RAD                            
044890           END-IF                                                         
045402           WRITE DOP-POST FROM MAIL-BLANKRAD                              
045502           WRITE DOP-POST FROM MAIL-SEPARATOR                             
045503           WRITE DOP-POST FROM MAIL-BLANKRAD                              
045504           WRITE DOP-POST FROM MAIL-END-1                                 
045602           WRITE DOP-POST FROM MAIL-END-2                                 
045702                                                                          
045802        END-IF                                                            
045902                                                                          
046002     END-PERFORM                                                          
046102                                                                          
046202                                                                          
046302     PERFORM Z-FINIT                                                      
046402                                                                          
046502     MOVE ZERO TO RETURN-CODE                                             
046602     GOBACK                                                               
046702     .                                                                    
046802                                                                          
046902                                                                          
047002 A-INIT SECTION.                                                          
047102     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
047202                                                                          
047302     OPEN INPUT  W4635D                                                   
047402     OPEN OUTPUT W4635E                                                   
047502                                                                          
047602     ACCEPT DAGENS-DATUM  FROM DATE                                       
047702     ACCEPT DAGENS-TID    FROM TIME                                       
047802     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
047902     .                                                                    
048002                                                                          
048102                                                                          
048202 B-NYTT-IDPRODNR SECTION.                                                 
048302     MOVE 'B-IDPRODNR      ' TO CURRENT-SECTION                           
048402                                                                          
048602     MOVE RAD-IDPRODNR     TO CURR-IDPRODNR                               
048702                                                                          
048802     MOVE RAD-IDSUPREF     TO CURR-IDSUPREF                               
049002     MOVE RAD-DAREGDAT     TO CURR-DAREGDAT                               
049003     MOVE ZERO             TO CURR-IDKOLLI                                
049102                                                                          
049202     .                                                                    
049302                                                                          
049402                                                                          
049502 C-SKRIV-HEADER  SECTION.                                                 
049602     MOVE 'C-SKRIV-HEADER  ' TO CURRENT-SECTION                           
049702                                                                          
049802     MOVE '¤DAPDDGSVIKTFEL' TO W001-DAP                                   
049902     WRITE DOP-POST FROM W001-DAP                                         
050002                                                                          
050102     MOVE SPACE            TO W001-DAP                                    
050202*    STRING '¤DAP'                                                        
050203     STRING '¤DAP' RAD-IDLEVNR                                            
050302     DELIMITED BY SIZE   INTO W001-DAP                                    
050402     WRITE DOP-POST FROM W001-DAP                                         
050502                                                                          
050602     MOVE RAD-IDLEVNR      TO INTRO-IDLEVNR                               
050702     WRITE DOP-POST FROM MAIL-INTRO-1                                     
050802     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
050902     WRITE DOP-POST FROM MAIL-INTRO-2                                     
051002     MOVE DAGENS-DATUM     TO INTRO-DATE                                  
051102     STRING DAGENS-TID(1:2) ':' DAGENS-TID(3:2)                           
051202            DELIMITED BY SIZE                                             
051302                          INTO INTRO-TIME                                 
051402     WRITE DOP-POST FROM MAIL-INTRO-3                                     
051502     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
051602     WRITE DOP-POST FROM MAIL-INTRO-3A                                    
051702     WRITE DOP-POST FROM MAIL-INTRO-3B                                    
051802     WRITE DOP-POST FROM MAIL-INTRO-3C                                    
051902     WRITE DOP-POST FROM MAIL-INTRO-3D                                    
052002     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
052102     WRITE DOP-POST FROM MAIL-INTRO-3E                                    
052202     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
052302     WRITE DOP-POST FROM MAIL-INTRO-4                                     
052402                                                                          
052502     MOVE CURR-IDPRODNR    TO MAIL-IDPRODNR                               
052602     WRITE DOP-POST FROM MAIL-RUBRIK-1                                    
052603     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
052605     MOVE RAD-VKORDBTO-KOLLI     TO MAIL-VKORDBTO                         
052702     WRITE DOP-POST FROM MAIL-RUBRIK-2                                    
052703     MOVE RAD-VKORDBTO-KOLLI-ADJ  TO MAIL-VKORDBTO-ADJ                    
053102     WRITE DOP-POST FROM MAIL-RUBRIK-3                                    
053202     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
053302                                                                          
053402     MOVE CURR-IDSUPREF    TO MAIL-IDSUPREF                               
053502     WRITE DOP-POST FROM MAIL-REFERENS                                    
053602                                                                          
053702     MOVE CURR-DAREGDAT    TO MAIL-DAREGDAT                               
053802     WRITE DOP-POST FROM MAIL-DATUM                                       
054102     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
054202                                                                          
054302     .                                                                    
054402                                                                          
054502                                                                          
054602 D-NY-ORDER SECTION.                                                      
054702     MOVE 'C-ORDER         ' TO CURRENT-SECTION                           
054802                                                                          
054902                                                                          
055002     MOVE RAD-IDLEVNR      TO MAIL-IDLEVNR                                
055102     IF RAD-IDDISTR = ALL '+'                                             
055202        MOVE ZERO          TO MAIL-IDDISTR                                
055302     ELSE                                                                 
055402        MOVE RAD-IDDISTR   TO MAIL-IDDISTR                                
055502     END-IF                                                               
055602     MOVE RAD-IDKUNDNR     TO MAIL-IDKUNDNR                               
055702     MOVE RAD-IDORDNR7     TO MAIL-IDORDNR                                
055703                              CURR-IDORDNR7                               
055802     IF RAD-IDDC = ALL '+'                                                
055902        MOVE SPACE         TO MAIL-IDDC                                   
056002     ELSE                                                                 
056102       MOVE RAD-IDDC       TO MAIL-IDDC                                   
056202     END-IF                                                               
056302     WRITE DOP-POST FROM MAIL-ORDER                                       
056402                                                                          
056502     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
056602     .                                                                    
056702                                                                          
056802                                                                          
056902 E-NYTT-KOLLI SECTION.                                                    
057002     MOVE 'E-NYTT-KOLLI    ' TO CURRENT-SECTION                           
057102                                                                          
057202                                                                          
057502     MOVE RAD-IDKOLLI        TO MAIL-IDKOLLI                              
057702                                CURR-IDKOLLI                              
057703     MOVE ZERO               TO CURR-VKART-SUM                            
057802     WRITE DOP-POST FROM MAIL-KOLLI                                       
057902     WRITE DOP-POST FROM MAIL-RADRUBRIK                                   
059302     .                                                                    
059402                                                                          
060002                                                                          
060003 F-NY-RAD SECTION.                                                        
060004     MOVE 'E-NYTT-KOLLI    ' TO CURRENT-SECTION                           
060005                                                                          
060006                                                                          
060030     MOVE SPACE                    TO MAIL-RAD                            
060040     MOVE RAD-IDRADNR              TO MAIL-IDRADNR                        
060050     MOVE RAD-IDARTNR              TO MAIL-IDARTNR                        
060060     MOVE RAD-KVLEVART             TO MAIL-KVLEVART                       
060070     COMPUTE CURR-VKART = (RAD-KVLEVART * RAD-VKART) / 1000               
060071     MOVE CURR-VKART               TO MAIL-VKART                          
060072     ADD CURR-VKART TO CURR-VKART-SUM                                     
060080     WRITE DOP-POST FROM MAIL-RAD                                         
060200     .                                                                    
060300                                                                          
060400                                                                          
065602                                                                          
065702                                                                          
065802 Z-FINIT SECTION.                                                         
065902     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
066002                                                                          
066102     CLOSE W4635D W4635E                                                  
066202                                                                          
066302     MOVE 'S' TO POSTSUM-OPKOD                                            
066402     CALL POSTSUM USING POSTSUM-PARM                                      
066502     .                                                                    
066602                                                                          
066702                                                                          
066802 S01-LAES-W4635D  SECTION.                                                
066902                                                                          
067002     READ W4635D INTO W4635D-AREA                                         
067102     AT END                                                               
067202        MOVE HIGH-VALUE TO W4635D-AREA                                    
067302        SET END-OF-W4635D TO TRUE                                         
067402                                                                          
067502     NOT AT END                                                           
067602        MOVE 'W4635D'   TO POSTSUM-FDNAMN                                 
067702        MOVE 'W4635CD1' TO POSTSUM-DDNAMN2                                
067802        MOVE SPACE      TO POSTSUM-TRANSTYP                               
067902        CALL POSTSUM USING POSTSUM-PARM                                   
068002     END-READ                                                             
068102     .                                                                    
068202                                                                          
068302                                                                          
