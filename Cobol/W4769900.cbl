000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4769900.                                                
000003 AUTHOR.         MOGREN STINA.                                            
000004 DATE-WRITTEN.   04/01/16.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*                                                                         
000008*    FUNKTION:                                                            
000009*        PROGRAMMET SKAPAR EN EXCEL-FIL                                   
000010*        MED UPPGIFTER OM FAKTUROR TILL GREKLAND                          
000011*                                                                         
000012*                                                                         
000013*    ABENDKODER:                                                          
000014*        U0016 -  . . . .                                                 
000015*        U1000 -  . . . .                                                 
000016*                                                                         
000017                                                                          
000018     SKIP3                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000023 FILE-CONTROL.                                                            
000024     SKIP2                                                                
000025*          --- FAKTURAINFO-FIL FRÅN W476D5                                
000026     SELECT W4765G                     ASSIGN TO W47699D1.                
000027     SKIP2                                                                
000028*          --- FIL I EXCEL-FORMAT                                         
000029     SELECT W47699                     ASSIGN TO W47699D2.                
000030     EJECT                                                                
000031 DATA DIVISION.                                                           
000032     SKIP3                                                                
000033 FILE SECTION.                                                            
000034     SKIP3                                                                
000035 FD  W4765G                                                               
000036     RECORDING       F                                                    
000037     BLOCK CONTAINS  0.                                                   
000038                                                                          
000039*01  -COPY W476GRK      -L.                                               
000040     SKIP3                                                                
000041 FD  W47699                                                               
000042     RECORDING       V                                                    
000043     BLOCK CONTAINS  0.                                                   
000044                                                                          
000045 01  MAIL-POST                PIC X(214).                                 
000046     EJECT                                                                
000047 WORKING-STORAGE SECTION.                                                 
000048                                                                          
000049 77  IDPGM                       PIC X(8)    VALUE 'W4769900'.            
000050 77  JA                          PIC X       VALUE 'J'.                   
000051 77  NEJ                         PIC X       VALUE 'N'.                   
000052 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
000053 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
000054 77  WS-KDVALISO                 PIC X(3)    VALUE SPACES.                
000055                                                                          
000056 77  W4765G-EOF-SW               PIC X       VALUE 'N'.                   
000057     88  END-OF-W4765G                       VALUE 'J'.                   
000058     EJECT                                                                
000066 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000067 01  FILLER REDEFINES DAGENS-DATUM.                                       
000068     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000069     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000070     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000071     EJECT                                                                
000072 01  W-BELOPP                   PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000073 01  W-BELOPP-EUR               PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000074 01  WS-BELOPP-FAKTURA          PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000075 01  WS-BELOPP-FAKTURA-SEK-EUR  PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000076 01  WS-BELOPP-FAKTURA-EUR      PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000077 01  WS-BELOPP-FAKTURA-DIFF     PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000078                                                                          
000079 01  SPAR-IDDC                  PIC X(2)     VALUE SPACE.                 
000080 01  SPAR-IDFAKT                PIC S9(7)    VALUE ZERO COMP-3.           
000081 01  SPAR-DAFINDOC              PIC 9(8)     VALUE ZERO.                  
000082 01  SPAR-IDSHIPM               PIC 9(7)     VALUE ZERO.                  
000083 01  SPAR-TISKEPPN              PIC S9(7)    VALUE ZERO COMP-3.           
000084 01  SPAR-IDKUNDNR              PIC S9(7)    VALUE ZERO COMP-3.           
000085 01  SPAR-IDORDNR7              PIC S9(7)    VALUE ZERO COMP-3.           
000086                                                                          
000087 01  DYNAMISKA-SUBPROGRAM.                                                
000088*                                                                         
000089     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000090     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000091     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000092     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000093     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000094     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
000095     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000096     SKIP2                                                                
000097*    --- PARAMETRAR TILL ABEND                                            
000098                                                                          
000099 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000101 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000102     SKIP2                                                                
000103*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
000104*01 -COPY W335CURR                                                        
000105     EJECT                                                                
000106*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
000107*01 -COPY W510CURR                                                        
000108     EJECT                                                                
000109 01  FELTEXT.                                                             
000110     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000111     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000112     EJECT                                                                
000113*    --- PARAMETRAR TILL POSTSUM                                          
000114*                                                                         
000115*01  -COPY W0005   -PRE  POSTSUM-                                         
000116     EJECT                                                                
000117*01  -COPY WDATAREA                                                       
000118     EJECT                                                                
000119 01  GRK-AREA-START              PIC X(24)   VALUE                        
000120                                 'GRK-AREA-START  '.                      
000121     SKIP2                                                                
000122                                                                          
000123*01   -COPY W476GRK                                                       
000124     EJECT                                                                
000125 01  MAIL-EXCEL-RUBRIK.                                                   
000126     03  FILLER  PIC X(3)  VALUE ' DC'.                                   
000127     03  FILLER  PIC X     VALUE X'05'.                                   
000128     03  FILLER  PIC X(10) VALUE '   Invoice'.                            
000129     03  FILLER  PIC X     VALUE X'05'.                                   
000130     03  FILLER  PIC X(10) VALUE '   Invoice'.                            
000131     03  FILLER  PIC X     VALUE X'05'.                                   
000132     03  FILLER  PIC X(9)  VALUE '   Dealer'.                             
000133     03  FILLER  PIC X     VALUE X'05'.                                   
000134     03  FILLER  PIC X(8)  VALUE '  Order'.                               
000135     03  FILLER  PIC X     VALUE X'05'.                                   
000136     03  FILLER  PIC X(7)  VALUE '  Order'.                               
000137     03  FILLER  PIC X     VALUE X'05'.                                   
000138     03  FILLER  PIC X(11) VALUE '      Part '.                           
000139     03  FILLER  PIC X     VALUE X'05'.                                   
000140     03  FILLER  PIC X(12) VALUE '   Price SEK'.                          
000141     03  FILLER  PIC X     VALUE X'05'.                                   
000142     03  FILLER  PIC X(8)  VALUE 'Quantity'.                              
000143     03  FILLER  PIC X     VALUE X'05'.                                   
000144     03  FILLER  PIC X(12) VALUE ' Total price'.                          
000145     03  FILLER  PIC X     VALUE X'05'.                                   
000146     03  FILLER  PIC X(9)  VALUE 'Shipping'.                              
000147     03  FILLER  PIC X     VALUE X'05'.                                   
000148     03  FILLER  PIC X(10) VALUE 'Shipping'.                              
000149     03  FILLER  PIC X     VALUE X'05'.                                   
000150     03  FILLER  PIC X(10) VALUE 'Country of'.                            
000151     03  FILLER  PIC X     VALUE X'05'.                                   
000152     03  FILLER  PIC X(11) VALUE 'Statistical'.                           
000153     03  FILLER  PIC X     VALUE X'05'.                                   
000154     03  FILLER  PIC X(10) VALUE 'Net weight'.                            
000155     03  FILLER  PIC X     VALUE X'05'.                                   
000156     03  FILLER  PIC X(7)  VALUE 'Freight'.                               
000157     03  FILLER  PIC X     VALUE X'05'.                                   
000158     03  FILLER  PIC X(15) VALUE 'Dealer Ref.   '.                        
000159     03  FILLER  PIC X     VALUE X'05'.                                   
000160     03  FILLER  PIC X(17) VALUE 'IDVIN            '.                     
000161     03  FILLER  PIC X     VALUE X'05'.                                   
000162     03  FILLER  PIC X(17) VALUE 'VAT Reg.no.      '.                     
000163     03  FILLER  PIC X     VALUE X'05'.                                   
000164*                                                                         
000165 01  MAIL-EXCEL-RUBRIK2.                                                  
000166     03  FILLER  PIC X(3)  VALUE SPACE.                                   
000167     03  FILLER  PIC X     VALUE X'05'.                                   
000168     03  FILLER  PIC X(10) VALUE '    number'.                            
000169     03  FILLER  PIC X     VALUE X'05'.                                   
000170     03  FILLER  PIC X(10) VALUE '      date'.                            
000171     03  FILLER  PIC X     VALUE X'05'.                                   
000172     03  FILLER  PIC X(9)  VALUE '   number'.                             
000173     03  FILLER  PIC X     VALUE X'05'.                                   
000174     03  FILLER  PIC X(8)  VALUE '  number'.                              
000175     03  FILLER  PIC X     VALUE X'05'.                                   
000176     03  FILLER  PIC X(7)  VALUE '  class'.                               
000177     03  FILLER  PIC X     VALUE X'05'.                                   
000178     03  FILLER  PIC X(11) VALUE '    number'.                            
000179     03  FILLER  PIC X     VALUE X'05'.                                   
000180     03  FILLER  PIC X(12) VALUE space.                                   
000181     03  FILLER  PIC X     VALUE X'05'.                                   
000182     03  FILLER  PIC X(8)  VALUE space.                                   
000183     03  FILLER  PIC X     VALUE X'05'.                                   
000184     03  FILLER  PIC X(12) VALUE ' EUR '.                                 
000185     03  FILLER  PIC X     VALUE X'05'.                                   
000186     03  FILLER  PIC X(9)  VALUE '  number'.                              
000187     03  FILLER  PIC X     VALUE X'05'.                                   
000188     03  FILLER  PIC X(10) VALUE '    date'.                              
000189     03  FILLER  PIC X     VALUE X'05'.                                   
000190     03  FILLER  PIC X(10) VALUE 'origin'.                                
000191     03  FILLER  PIC X     VALUE X'05'.                                   
000192     03  FILLER  PIC X(11) VALUE '     number'.                           
000193     03  FILLER  PIC X     VALUE X'05'.                                   
000194     03  FILLER  PIC X(10) VALUE space.                                   
000195     03  FILLER  PIC X     VALUE X'05'.                                   
000196     03  FILLER  PIC X(7)  VALUE '   code'.                               
000197     03  FILLER  PIC X     VALUE X'05'.                                   
000198     03  FILLER  PIC X(15) VALUE ' number  '.                             
000199     03  FILLER  PIC X     VALUE X'05'.                                   
000200     03  FILLER  PIC X(17)  VALUE ' number          '.                    
000201     03  FILLER  PIC X     VALUE X'05'.                                   
000202     03  FILLER  PIC X(17)  VALUE SPACE.                                  
000203     03  FILLER  PIC X     VALUE X'05'.                                   
000204*                                                                         
000205 01  FILLER                      PIC X(24)   VALUE                        
000206                                 'UT-AREA-START  '.                       
000207 01  UT-AREA.                                                             
000208     03  FILLER            PIC X   VALUE SPACE.                           
000209     03  UT-IDDC           PIC XX  VALUE SPACE.                           
000210     03  FILLER            PIC X   VALUE X'05'.                           
000211     03  UT-IDFAKT         PIC Z(9)9.                                     
000212     03  FILLER            PIC X   VALUE X'05'.                           
000213     03  UT-DAFINDOC       PIC Z(9)9.                                     
000214     03  FILLER            PIC X   VALUE X'05'.                           
000215     03  UT-IDKUNDNR       PIC Z(8)9.                                     
000216     03  FILLER            PIC X   VALUE X'05'.                           
000217     03  UT-IDORDNR        PIC Z(7)9.                                     
000218     03  FILLER            PIC X   VALUE X'05'.                           
000219     03  UT-KDORDKL        PIC Z(6)9.                                     
000220     03  FILLER            PIC X   VALUE X'05'.                           
000221     03  UT-IDARTNR        PIC Z(10)9.                                    
000222     03  FILLER            PIC X   VALUE X'05'.                           
000223     03  UT-PRARTNTO       PIC Z(8)9.99.                                  
000224     03  FILLER            PIC X   VALUE X'05'.                           
000225     03  UT-KVLEVART       PIC Z(7)9.                                     
000226     03  FILLER            PIC X   VALUE X'05'.                           
000227     03  UT-RADTOT         PIC -(8)9.99.                                  
000228     03  FILLER            PIC X   VALUE X'05'.                           
000229     03  UT-IDSHIPM        PIC Z(8)9.                                     
000230     03  FILLER            PIC X   VALUE X'05'.                           
000231     03  UT-TISKEPPN       PIC Z(9)9.                                     
000232     03  FILLER            PIC X   VALUE X'05'.                           
000233     03  UT-KDARTURS       PIC X(2)B(8).                                  
000234     03  FILLER            PIC X   VALUE X'05'.                           
000235     03  UT-IDSTATNR       PIC Z(10)9.                                    
000236     03  FILLER            PIC X   VALUE X'05'.                           
000237     03  UT-VKARTNTO       PIC Z(5)9.999.                                 
000238     03  FILLER            PIC X   VALUE X'05'.                           
000239     03  UT-KDFRAKT        PIC Z(6)9.                                     
000240     03  FILLER            PIC X   VALUE X'05'.                           
000241     03  UT-BEKUNDRF       PIC X(15).                                     
000242     03  FILLER            PIC X   VALUE X'05'.                           
000243     03  UT-IDVIN          PIC X(17).                                     
000244     03  FILLER            PIC X   VALUE X'05'.                           
000245     03  UT-IDVAT          PIC X(17).                                     
000246     03  FILLER            PIC X   VALUE X'05'.                           
000247     SKIP2                                                                
000248 01  UTK-AREA.                                                            
000249     03  FILLER            PIC X(3) VALUE SPACE.                          
000250     03  FILLER            PIC X   VALUE X'05'.                           
000251     03  FILLER            PIC X(10) VALUE SPACE.                         
000252     03  FILLER            PIC X   VALUE X'05'.                           
000253     03  FILLER            PIC X(10) VALUE SPACE.                         
000254     03  FILLER            PIC X   VALUE X'05'.                           
000255     03  UTK-IDKUNDNR      PIC Z(8)9.                                     
000256     03  FILLER            PIC X   VALUE X'05'.                           
000257     03  FILLER            PIC X(8)  VALUE SPACE.                         
000258     03  FILLER            PIC X   VALUE X'05'.                           
000259     03  FILLER            PIC X(7) VALUE SPACE.                          
000260     03  FILLER            PIC X   VALUE X'05'.                           
000261     03  FILLER            PIC X(11) VALUE SPACE.                         
000262     03  FILLER            PIC X   VALUE X'05'.                           
000263     03  UTK-BELOPP        PIC Z(8)9.99.                                  
000264     03  FILLER            PIC X   VALUE X'05'.                           
000265     03  FILLER            PIC X(8)  VALUE SPACE.                         
000266     03  FILLER            PIC X   VALUE X'05'.                           
000267     03  UTK-BELOPPT       PIC Z(8)9.99.                                  
000268     03  FILLER            PIC X   VALUE X'05'.                           
000269     03  FILLER            PIC X(9) VALUE SPACE.                          
000270     03  FILLER            PIC X   VALUE X'05'.                           
000271     03  FILLER            PIC X(10) VALUE SPACE.                         
000272     03  FILLER            PIC X   VALUE X'05'.                           
000273     03  FILLER            PIC X(2)B(8) VALUE SPACE.                      
000274     03  FILLER            PIC X   VALUE X'05'.                           
000275     03  FILLER            PIC X(11) VALUE SPACE.                         
000276     03  FILLER            PIC X   VALUE X'05'.                           
000277     03  FILLER            PIC X(10) VALUE SPACE.                         
000278     03  FILLER            PIC X   VALUE X'05'.                           
000279     03  FILLER            PIC X(7) VALUE SPACE.                          
000280     03  FILLER            PIC X   VALUE X'05'.                           
000281 01  UTT-AREA.                                                            
000282     03  FILLER            PIC X(3) VALUE SPACE.                          
000283     03  FILLER            PIC X   VALUE X'05'.                           
000284     03  FILLER            PIC X(10) VALUE SPACE.                         
000285     03  FILLER            PIC X   VALUE X'05'.                           
000286     03  FILLER            PIC X(10) VALUE SPACE.                         
000287     03  FILLER            PIC X   VALUE X'05'.                           
000288     03  FILLER            PIC X(9) VALUE SPACE.                          
000289     03  FILLER            PIC X   VALUE X'05'.                           
000290     03  FILLER            PIC X(8)  VALUE SPACE.                         
000291     03  FILLER            PIC X   VALUE X'05'.                           
000292     03  FILLER            PIC X(7) VALUE SPACE.                          
000293     03  FILLER            PIC X   VALUE X'05'.                           
000294     03  FILLER            PIC X(11) VALUE SPACE.                         
000295     03  FILLER            PIC X   VALUE X'05'.                           
000296     03  UTT-BELOPP        PIC Z(8)9.99.                                  
000297     03  FILLER            PIC X   VALUE X'05'.                           
000298     03  FILLER            PIC X(8)  VALUE SPACE.                         
000299     03  FILLER            PIC X   VALUE X'05'.                           
000300     03  UTT-BELOPPT       PIC Z(8)9.99.                                  
000301     03  FILLER            PIC X   VALUE X'05'.                           
000302     03  FILLER            PIC X(9) VALUE SPACE.                          
000303     03  FILLER            PIC X   VALUE X'05'.                           
000304     03  FILLER            PIC X(10) VALUE SPACE.                         
000305     03  FILLER            PIC X   VALUE X'05'.                           
000306     03  FILLER            PIC X(2)B(8) VALUE SPACE.                      
000307     03  FILLER            PIC X   VALUE X'05'.                           
000308     03  FILLER            PIC X(11) VALUE SPACE.                         
000309     03  FILLER            PIC X   VALUE X'05'.                           
000310     03  FILLER            PIC X(10) VALUE SPACE.                         
000311     03  FILLER            PIC X   VALUE X'05'.                           
000312     03  FILLER            PIC X(7) VALUE SPACE.                          
000313     03  FILLER            PIC X   VALUE X'05'.                           
000314                                                                          
000315     EJECT                                                                
000316*                                                                         
000317 LINKAGE SECTION.                                                         
000318*01  -COPY W0008      -PRE WDG2-                                          
000319     05  FILLER                  PIC X.                                   
000320     EJECT                                                                
000321 PROCEDURE DIVISION   USING  WDG2-PCB.                                    
000322                                                                          
000323 MAIN SECTION.                                                            
000324     ENTRY 'DLITCBL'  USING  WDG2-PCB.                                    
000325                                                                          
000326     PERFORM A-INIT                                                       
000327     PERFORM S01-LAES-W4765G                                              
000328     IF NOT END-OF-W4765G                                                 
000329       MOVE GRK-IDDC     TO SPAR-IDDC                                     
000330       MOVE GRK-IDFAKT   TO SPAR-IDFAKT                                   
000331       MOVE GRK-DAFINDOC TO SPAR-DAFINDOC                                 
000332       MOVE GRK-IDSHIPM  TO SPAR-IDSHIPM                                  
000333       MOVE GRK-TISKEPPN TO SPAR-TISKEPPN                                 
000334       MOVE GRK-IDKUNDNR TO SPAR-IDKUNDNR                                 
000335       MOVE GRK-IDORDNR7 TO SPAR-IDORDNR7                                 
000336                                                                          
000337       PERFORM B-GET-CURRENCY                                             
000338     END-IF                                                               
000339     PERFORM UNTIL  END-OF-W4765G                                         
000340       IF GRK-IDFAKT = SPAR-IDFAKT                                        
000341         PERFORM F-SKRIV-RAD                                              
000342       ELSE                                                               
000343         PERFORM G-CHECK-CURRENCY-DIFF                                    
000344         MOVE GRK-IDDC     TO SPAR-IDDC                                   
000345         MOVE GRK-IDFAKT   TO SPAR-IDFAKT                                 
000346         MOVE GRK-DAFINDOC TO SPAR-DAFINDOC                               
000347         MOVE GRK-IDSHIPM  TO SPAR-IDSHIPM                                
000348         MOVE GRK-TISKEPPN TO SPAR-TISKEPPN                               
000349         MOVE GRK-IDKUNDNR TO SPAR-IDKUNDNR                               
000350         MOVE GRK-IDORDNR7 TO SPAR-IDORDNR7                               
000351         PERFORM F-SKRIV-RAD                                              
000352       END-IF                                                             
000353       PERFORM S01-LAES-W4765G                                            
000354     END-PERFORM                                                          
000355     PERFORM G-CHECK-CURRENCY-DIFF                                        
000356                                                                          
000357     PERFORM Z-FINIT                                                      
000358                                                                          
000359     MOVE ZERO TO RETURN-CODE                                             
000360     GOBACK                                                               
000361     .                                                                    
000362     EJECT                                                                
000363                                                                          
000364 A-INIT SECTION.                                                          
000365     OPEN INPUT  W4765G                                                   
000366     OPEN OUTPUT W47699                                                   
000367                                                                          
000368     ACCEPT DAGENS-DATUM  FROM DATE                                       
000369     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000370                                                                          
000371     WRITE MAIL-POST    FROM MAIL-EXCEL-RUBRIK                            
000372     WRITE MAIL-POST    FROM MAIL-EXCEL-RUBRIK2                           
000373                                                                          
000374     MOVE 'W47699'      TO POSTSUM-FDNAMN                                 
000375     MOVE 'W47699D2'    TO POSTSUM-DDNAMN2                                
000376     CALL POSTSUM       USING POSTSUM-PARM                                
000377     .                                                                    
000378     EJECT                                                                
000379                                                                          
000380 B-GET-CURRENCY SECTION.                                                  
000381                                                                          
000382     MOVE GRK-DAFINDOC(3:2)  TO W-DATE-AAMM(1:2)                          
000384     MOVE GRK-DAFINDOC(5:2)  TO W-DATE-AAMM(3:2)                          
000385     MOVE W-DATE-AAMM        TO CURR-TIAAMM                               
000386     MOVE 'EUR'              TO WS-KDVALISO                               
000387                                CURR-KDVALISO-ROW                         
000388     MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                         
000389     MOVE 'M'                TO CURR-KDVALTYP                             
000390     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
000391     IF CURR-KDSVAR = ' '                                                 
000392        CONTINUE                                                          
000393     ELSE                                                                 
000394        MOVE 1               TO CURR-PRKURS-NEW                           
000395     END-IF                                                               
000396     .                                                                    
000397     EJECT                                                                
000398                                                                          
000399 F-SKRIV-RAD  SECTION.                                                    
000400     MOVE GRK-IDDC           TO UT-IDDC                                   
000401     MOVE GRK-IDFAKT         TO UT-IDFAKT                                 
000402     MOVE GRK-DAFINDOC       TO UT-DAFINDOC                               
000403     MOVE GRK-IDSHIPM        TO UT-IDSHIPM                                
000404     MOVE GRK-TISKEPPN       TO UT-TISKEPPN                               
000405     MOVE GRK-IDKUNDNR       TO UT-IDKUNDNR                               
000406     MOVE GRK-IDORDNR7       TO UT-IDORDNR                                
000407     MOVE GRK-KDORDKL        TO UT-KDORDKL                                
000408     MOVE GRK-KDFRAKT        TO UT-KDFRAKT                                
000409     MOVE GRK-IDARTNR        TO UT-IDARTNR                                
000410     MOVE GRK-KVLEVART       TO UT-KVLEVART                               
000411     MOVE GRK-KDARTURS       TO UT-KDARTURS                               
000412     MOVE GRK-IDSTATNR       TO UT-IDSTATNR                               
000413     MOVE GRK-VKARTNTO       TO UT-VKARTNTO                               
000414     MOVE GRK-PRARTNTO       TO UT-PRARTNTO                               
000415     MOVE GRK-BEKUNDRF       TO UT-BEKUNDRF                               
000416     MOVE GRK-IDVIN          TO UT-IDVIN                                  
000417     MOVE GRK-IDVAT          TO UT-IDVAT                                  
000418                                                                          
000419     PERFORM FA-SUMMERA-BELOPP                                            
000420                                                                          
000421     WRITE MAIL-POST  FROM UT-AREA                                        
000422                                                                          
000423     MOVE 'W47699'    TO POSTSUM-FDNAMN                                   
000424     MOVE 'W47699D2'  TO POSTSUM-DDNAMN2                                  
000425     CALL POSTSUM   USING POSTSUM-PARM                                    
000426     .                                                                    
000427     EJECT                                                                
000428                                                                          
000429 FA-SUMMERA-BELOPP  SECTION.                                              
000430     COMPUTE W-BELOPP ROUNDED = GRK-KVLEVART * GRK-PRARTNTO               
000431     IF GRK-IDARTNR = ZERO                                                
000432        MOVE GRK-PRARTNTO    TO W-BELOPP                                  
000433     END-IF                                                               
000434****                                                                      
000435     ADD W-BELOPP            TO WS-BELOPP-FAKTURA                         
000436                                                                          
000437     PERFORM H-BERAKNA-VALUTA                                             
000438     MOVE W-BELOPP-EUR       TO UT-RADTOT                                 
000439     ADD  W-BELOPP-EUR       TO WS-BELOPP-FAKTURA-EUR                     
000440     .                                                                    
000441     EJECT                                                                
000442                                                                          
000443 G-CHECK-CURRENCY-DIFF SECTION.                                           
000444     MOVE WS-BELOPP-FAKTURA TO W-BELOPP                                   
000445     PERFORM H-BERAKNA-VALUTA                                             
000446     MOVE W-BELOPP-EUR       TO WS-BELOPP-FAKTURA-SEK-EUR                 
000447                                                                          
000448     IF WS-BELOPP-FAKTURA-EUR = WS-BELOPP-FAKTURA-SEK-EUR                 
000449       CONTINUE                                                           
000450     ELSE                                                                 
000451       MOVE SPAR-IDDC        TO UT-IDDC                                   
000452       MOVE SPAR-IDFAKT      TO UT-IDFAKT                                 
000453       MOVE SPAR-DAFINDOC    TO UT-DAFINDOC                               
000454       MOVE SPAR-IDSHIPM     TO UT-IDSHIPM                                
000455       MOVE SPAR-TISKEPPN    TO UT-TISKEPPN                               
000456       MOVE SPAR-IDKUNDNR    TO UT-IDKUNDNR                               
000457       MOVE ZERO             TO UT-IDORDNR                                
000458       MOVE ZERO             TO UT-KDORDKL                                
000459       MOVE ZERO             TO UT-KDFRAKT                                
000460       MOVE ZERO             TO UT-IDARTNR                                
000461       MOVE 1                TO UT-KVLEVART                               
000462       MOVE SPACE            TO UT-KDARTURS                               
000463       MOVE SPACE            TO UT-BEKUNDRF                               
000464       MOVE SPACE            TO UT-IDVIN                                  
000465       MOVE ZERO             TO UT-IDSTATNR                               
000466       MOVE ZERO             TO UT-VKARTNTO                               
000467       MOVE ZERO             TO UT-PRARTNTO                               
000468       MOVE SPACE            TO UT-IDVAT                                  
000469**** BERÄKNA DIFFERANS                                                    
000470       COMPUTE WS-BELOPP-FAKTURA-DIFF =                                   
000471               WS-BELOPP-FAKTURA-SEK-EUR - WS-BELOPP-FAKTURA-EUR          
000472       MOVE WS-BELOPP-FAKTURA-DIFF TO UT-RADTOT                           
000473                                                                          
000474       WRITE MAIL-POST FROM UT-AREA                                       
000475                                                                          
000476       MOVE 'W47699'  TO POSTSUM-FDNAMN                                   
000477       MOVE 'W47699D2' TO POSTSUM-DDNAMN2                                 
000478       CALL POSTSUM USING POSTSUM-PARM                                    
000479     END-IF                                                               
000480     .                                                                    
000481     EJECT                                                                
000482                                                                          
000483 H-BERAKNA-VALUTA  SECTION.                                               
000484     MOVE  ZERO                     TO CURR-SUORDV-IN                     
000485                                       CURR-PRARTVNA-IN                   
000486                                       CURR-PRARTSTD-IN                   
000487                                       CURR-PRKURS-02                     
000488     MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                        
000489     MOVE W-BELOPP                  TO CURR-PRARTSJK-IN                   
000490     MOVE WS-KDVALISO               TO CURR-KDVALISO-01                   
000491     MOVE +2                        TO CURR-KDCALL                        
000492     CALL W335CURR            USING CURR-W335CURR                         
000493     MOVE CURR-PRARTSJK-UT          TO W-BELOPP-EUR                       
000494     .                                                                    
000495     EJECT                                                                
000496                                                                          
000497 Z-FINIT SECTION.                                                         
000498     CLOSE W4765G                                                         
000499           W47699                                                         
000500     SKIP2                                                                
000501     MOVE 'S' TO POSTSUM-OPKOD                                            
000502     CALL POSTSUM USING POSTSUM-PARM                                      
000503     .                                                                    
000504     EJECT                                                                
000505                                                                          
000506 S01-LAES-W4765G  SECTION.                                                
000507     READ W4765G INTO GRK-W476GRK                                         
000508     AT END                                                               
000509        MOVE HIGH-VALUE TO GRK-W476GRK                                    
000510        SET END-OF-W4765G TO TRUE                                         
000511                                                                          
000512     NOT AT END                                                           
000513        MOVE 'W4765G' TO POSTSUM-FDNAMN                                   
000514        MOVE 'W47699D1' TO POSTSUM-DDNAMN2                                
000515        MOVE SPACE      TO POSTSUM-TRANSTYP                               
000516        CALL POSTSUM USING POSTSUM-PARM                                   
000517     END-READ                                                             
000518     .                                                                    
000519     EJECT                                                                
000520                                                                          
000521 S99-ABEND SECTION.                                                       
000522     MOVE 'S' TO POSTSUM-OPKOD                                            
000523     CALL POSTSUM USING POSTSUM-PARM                                      
000524     CALL ABEND USING RKOD-ABEND                                          
000530     .                                                                    
