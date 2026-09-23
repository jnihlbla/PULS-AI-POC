000100* GENERATION OF COBOL HOST STRUCTURE FROM TZ4DIRU-TAB                     
000200  01 TZ4DIRU.                                                             
000300*              TZ4DIRU                                                    
000400   03 IDOUTTYPE        PIC X(15).                                         
000500*              OUTPUTTYP                                                  
000600   03 IDOUTREC-FROM    PIC X(30).                                         
000700*              OUTPUTMOTTAGARE (FR.O.M)                                   
000800   03 IDOUTREC-TO      PIC X(30).                                         
000900*              OUTPUTMOTTAGARE (T.O.M)                                    
001000   03 KDOUTMETH-1      PIC X(4).                                          
001100*              OUTPUTMETOD                                                
001200   03 KDOUTMETH-2      PIC X(4).                                          
001300*              OUTPUTMETOD                                                
001400   03 KDOUTMETH-3      PIC X(4).                                          
001500*              OUTPUTMETOD                                                
001600   03 KDOUTMETH-4      PIC X(4).                                          
001700*              OUTPUTMETOD                                                
001800   03 KDOUTMETH-5      PIC X(4).                                          
001900*              OUTPUTMETOD                                                
002000   03 KDOUTMETH-6      PIC X(4).                                          
002100*              OUTPUTMETOD                                                
002200   03 KDOUTMETH-7      PIC X(4).                                          
002300*              OUTPUTMETOD                                                
002400   03 KDOUTMETH-8      PIC X(4).                                          
002500*              OUTPUTMETOD                                                
002600   03 KDOUTMETH-9      PIC X(4).                                          
002700*              OUTPUTMETOD                                                
002800   03 KDOUTMETH-10     PIC X(4).                                          
002900*              OUTPUTMETOD                                                
003000   03 KDOUTMETH-11     PIC X(4).                                          
003100*              OUTPUTMETOD                                                
003200   03 KDOUTMETH-12     PIC X(4).                                          
003300*              OUTPUTMETOD                                                
003400   03 KDOUTMETH-13     PIC X(4).                                          
003500*              OUTPUTMETOD                                                
003600   03 KDOUTMETH-14     PIC X(4).                                          
003700*              OUTPUTMETOD                                                
003800   03 KDOUTMETH-15     PIC X(4).                                          
003900*              OUTPUTMETOD                                                
004000   03 IDOUTDEST-1      PIC X(60).                                         
004100*              FYSISK OUTPUT DESTINATION                                  
004200   03 IDOUTDEST-2      PIC X(60).                                         
004300*              FYSISK OUTPUT DESTINATION                                  
004400   03 IDOUTDEST-3      PIC X(60).                                         
004500*              FYSISK OUTPUT DESTINATION                                  
004600   03 IDOUTDEST-4      PIC X(60).                                         
004700*              FYSISK OUTPUT DESTINATION                                  
004800   03 IDOUTDEST-5      PIC X(60).                                         
004900*              FYSISK OUTPUT DESTINATION                                  
005000   03 IDOUTDEST-6      PIC X(60).                                         
005100*              FYSISK OUTPUT DESTINATION                                  
005200   03 IDOUTDEST-7      PIC X(60).                                         
005300*              FYSISK OUTPUT DESTINATION                                  
005400   03 IDOUTDEST-8      PIC X(60).                                         
005500*              FYSISK OUTPUT DESTINATION                                  
005600   03 IDOUTDEST-9      PIC X(60).                                         
005700*              FYSISK OUTPUT DESTINATION                                  
005800   03 IDOUTDEST-10     PIC X(60).                                         
005900*              FYSISK OUTPUT DESTINATION                                  
006000   03 IDOUTDEST-11     PIC X(60).                                         
006100*              FYSISK OUTPUT DESTINATION                                  
006200   03 IDOUTDEST-12     PIC X(60).                                         
006300*              FYSISK OUTPUT DESTINATION                                  
006400   03 IDOUTDEST-13     PIC X(60).                                         
006500*              FYSISK OUTPUT DESTINATION                                  
006600   03 IDOUTDEST-14     PIC X(60).                                         
006700*              FYSISK OUTPUT DESTINATION                                  
006800   03 IDOUTDEST-15     PIC X(60).                                         
006900*              FYSISK OUTPUT DESTINATION                                  
007000   03 KVCOPIES-1       PIC X(1).                                          
007100*              ANTAL COPIOR VID PRINTNING                                 
007200   03 KVCOPIES-2       PIC X(1).                                          
007300*              ANTAL COPIOR VID PRINTNING                                 
007400   03 KVCOPIES-3       PIC X(1).                                          
007500*              ANTAL COPIOR VID PRINTNING                                 
007600   03 KVCOPIES-4       PIC X(1).                                          
007700*              ANTAL COPIOR VID PRINTNING                                 
007800   03 KVCOPIES-5       PIC X(1).                                          
007900*              ANTAL COPIOR VID PRINTNING                                 
008000   03 KVCOPIES-6       PIC X(1).                                          
008100*              ANTAL COPIOR VID PRINTNING                                 
008200   03 KVCOPIES-7       PIC X(1).                                          
008300*              ANTAL COPIOR VID PRINTNING                                 
008400   03 KVCOPIES-8       PIC X(1).                                          
008500*              ANTAL COPIOR VID PRINTNING                                 
008600   03 KVCOPIES-9       PIC X(1).                                          
008700*              ANTAL COPIOR VID PRINTNING                                 
008800   03 KVCOPIES-10      PIC X(1).                                          
008900*              ANTAL COPIOR VID PRINTNING                                 
009000   03 KVCOPIES-11      PIC X(1).                                          
009100*              ANTAL COPIOR VID PRINTNING                                 
009200   03 KVCOPIES-12      PIC X(1).                                          
009300*              ANTAL COPIOR VID PRINTNING                                 
009400   03 KVCOPIES-13      PIC X(1).                                          
009500*              ANTAL COPIOR VID PRINTNING                                 
009600   03 KVCOPIES-14      PIC X(1).                                          
009700*              ANTAL COPIOR VID PRINTNING                                 
009800   03 KVCOPIES-15      PIC X(1).                                          
009900*              ANTAL COPIOR VID PRINTNING                                 
010000   03 FLCARRCNTL-1     PIC X(1).                                          
010100*              INGÅR STYRTECKEN I DATA?                                   
010200   03 FLCARRCNTL-2     PIC X(1).                                          
010300*              INGÅR STYRTECKEN I DATA?                                   
010400   03 FLCARRCNTL-3     PIC X(1).                                          
010500*              INGÅR STYRTECKEN I DATA?                                   
010600   03 FLCARRCNTL-4     PIC X(1).                                          
010700*              INGÅR STYRTECKEN I DATA?                                   
010800   03 FLCARRCNTL-5     PIC X(1).                                          
010900*              INGÅR STYRTECKEN I DATA?                                   
011000   03 FLCARRCNTL-6     PIC X(1).                                          
011100*              INGÅR STYRTECKEN I DATA?                                   
011200   03 FLCARRCNTL-7     PIC X(1).                                          
011300*              INGÅR STYRTECKEN I DATA?                                   
011400   03 FLCARRCNTL-8     PIC X(1).                                          
011500*              INGÅR STYRTECKEN I DATA?                                   
011600   03 FLCARRCNTL-9     PIC X(1).                                          
011700*              INGÅR STYRTECKEN I DATA?                                   
011800   03 FLCARRCNTL-10    PIC X(1).                                          
011900*              INGÅR STYRTECKEN I DATA?                                   
012000   03 FLCARRCNTL-11    PIC X(1).                                          
012100*              INGÅR STYRTECKEN I DATA?                                   
012200   03 FLCARRCNTL-12    PIC X(1).                                          
012300*              INGÅR STYRTECKEN I DATA?                                   
012400   03 FLCARRCNTL-13    PIC X(1).                                          
012500*              INGÅR STYRTECKEN I DATA?                                   
012600   03 FLCARRCNTL-14    PIC X(1).                                          
012700*              INGÅR STYRTECKEN I DATA?                                   
012800   03 FLCARRCNTL-15    PIC X(1).                                          
012900*              INGÅR STYRTECKEN I DATA?                                   
013000   03 IDPFDEF-1        PIC X(8).                                          
013100*              IBM PSF FORMSDEF,PAGEDEF                                   
013200   03 IDPFDEF-2        PIC X(8).                                          
013300*              IBM PSF FORMSDEF,PAGEDEF                                   
013400   03 IDPFDEF-3        PIC X(8).                                          
013500*              IBM PSF FORMSDEF,PAGEDEF                                   
013600   03 IDPFDEF-4        PIC X(8).                                          
013700*              IBM PSF FORMSDEF,PAGEDEF                                   
013800   03 IDPFDEF-5        PIC X(8).                                          
013900*              IBM PSF FORMSDEF,PAGEDEF                                   
014000   03 IDPFDEF-6        PIC X(8).                                          
014100*              IBM PSF FORMSDEF,PAGEDEF                                   
014200   03 IDPFDEF-7        PIC X(8).                                          
014300*              IBM PSF FORMSDEF,PAGEDEF                                   
014400   03 IDPFDEF-8        PIC X(8).                                          
014500*              IBM PSF FORMSDEF,PAGEDEF                                   
014600   03 IDPFDEF-9        PIC X(8).                                          
014700*              IBM PSF FORMSDEF,PAGEDEF                                   
014800   03 IDPFDEF-10       PIC X(8).                                          
014900*              IBM PSF FORMSDEF,PAGEDEF                                   
015000   03 IDPFDEF-11       PIC X(8).                                          
015100*              IBM PSF FORMSDEF,PAGEDEF                                   
015200   03 IDPFDEF-12       PIC X(8).                                          
015300*              IBM PSF FORMSDEF,PAGEDEF                                   
015400   03 IDPFDEF-13       PIC X(8).                                          
015500*              IBM PSF FORMSDEF,PAGEDEF                                   
015600   03 IDPFDEF-14       PIC X(8).                                          
015700*              IBM PSF FORMSDEF,PAGEDEF                                   
015800   03 IDPFDEF-15       PIC X(8).                                          
015900*              IBM PSF FORMSDEF,PAGEDEF                                   
016000   03 IDFORMSNM-1      PIC X(8).                                          
016100*              FORMS/BLANKETT-NAMN                                        
016200   03 IDFORMSNM-2      PIC X(8).                                          
016300*              FORMS/BLANKETT-NAMN                                        
016400   03 IDFORMSNM-3      PIC X(8).                                          
016500*              FORMS/BLANKETT-NAMN                                        
016600   03 IDFORMSNM-4      PIC X(8).                                          
016700*              FORMS/BLANKETT-NAMN                                        
016800   03 IDFORMSNM-5      PIC X(8).                                          
016900*              FORMS/BLANKETT-NAMN                                        
017000   03 IDFORMSNM-6      PIC X(8).                                          
017100*              FORMS/BLANKETT-NAMN                                        
017200   03 IDFORMSNM-7      PIC X(8).                                          
017300*              FORMS/BLANKETT-NAMN                                        
017400   03 IDFORMSNM-8      PIC X(8).                                          
017500*              FORMS/BLANKETT-NAMN                                        
017600   03 IDFORMSNM-9      PIC X(8).                                          
017700*              FORMS/BLANKETT-NAMN                                        
017800   03 IDFORMSNM-10     PIC X(8).                                          
017900*              FORMS/BLANKETT-NAMN                                        
018000   03 IDFORMSNM-11     PIC X(8).                                          
018100*              FORMS/BLANKETT-NAMN                                        
018200   03 IDFORMSNM-12     PIC X(8).                                          
018300*              FORMS/BLANKETT-NAMN                                        
018400   03 IDFORMSNM-13     PIC X(8).                                          
018500*              FORMS/BLANKETT-NAMN                                        
018600   03 IDFORMSNM-14     PIC X(8).                                          
018700*              FORMS/BLANKETT-NAMN                                        
018800   03 IDFORMSNM-15     PIC X(8).                                          
018900*              FORMS/BLANKETT-NAMN                                        
019000   03 TEVCOMST-1       PIC X(20).                                         
019100*              VCOM SENDERTAG                                             
019200   03 TEVCOMST-2       PIC X(20).                                         
019300*              VCOM SENDERTAG                                             
019400   03 TEVCOMST-3       PIC X(20).                                         
019500*              VCOM SENDERTAG                                             
019600   03 TEVCOMST-4       PIC X(20).                                         
019700*              VCOM SENDERTAG                                             
019800   03 TEVCOMST-5       PIC X(20).                                         
019900*              VCOM SENDERTAG                                             
020000   03 TEVCOMST-6       PIC X(20).                                         
020100*              VCOM SENDERTAG                                             
020200   03 TEVCOMST-7       PIC X(20).                                         
020300*              VCOM SENDERTAG                                             
020400   03 TEVCOMST-8       PIC X(20).                                         
020500*              VCOM SENDERTAG                                             
020600   03 TEVCOMST-9       PIC X(20).                                         
020700*              VCOM SENDERTAG                                             
020800   03 TEVCOMST-10      PIC X(20).                                         
020900*              VCOM SENDERTAG                                             
021000   03 TEVCOMST-11      PIC X(20).                                         
021100*              VCOM SENDERTAG                                             
021200   03 TEVCOMST-12      PIC X(20).                                         
021300*              VCOM SENDERTAG                                             
021400   03 TEVCOMST-13      PIC X(20).                                         
021500*              VCOM SENDERTAG                                             
021600   03 TEVCOMST-14      PIC X(20).                                         
021700*              VCOM SENDERTAG                                             
021800   03 TEVCOMST-15      PIC X(20).                                         
021900*              VCOM SENDERTAG                                             
022000   03 IDVCINIT-1       PIC X(8).                                          
022100*              VCOM INITIATOR PROGRAM NAMN                                
022200   03 IDVCINIT-2       PIC X(8).                                          
022300*              VCOM INITIATOR PROGRAM NAMN                                
022400   03 IDVCINIT-3       PIC X(8).                                          
022500*              VCOM INITIATOR PROGRAM NAMN                                
022600   03 IDVCINIT-4       PIC X(8).                                          
022700*              VCOM INITIATOR PROGRAM NAMN                                
022800   03 IDVCINIT-5       PIC X(8).                                          
022900*              VCOM INITIATOR PROGRAM NAMN                                
023000   03 IDVCINIT-6       PIC X(8).                                          
023100*              VCOM INITIATOR PROGRAM NAMN                                
023200   03 IDVCINIT-7       PIC X(8).                                          
023300*              VCOM INITIATOR PROGRAM NAMN                                
023400   03 IDVCINIT-8       PIC X(8).                                          
023500*              VCOM INITIATOR PROGRAM NAMN                                
023600   03 IDVCINIT-9       PIC X(8).                                          
023700*              VCOM INITIATOR PROGRAM NAMN                                
023800   03 IDVCINIT-10      PIC X(8).                                          
023900*              VCOM INITIATOR PROGRAM NAMN                                
024000   03 IDVCINIT-11      PIC X(8).                                          
024100*              VCOM INITIATOR PROGRAM NAMN                                
024200   03 IDVCINIT-12      PIC X(8).                                          
024300*              VCOM INITIATOR PROGRAM NAMN                                
024400   03 IDVCINIT-13      PIC X(8).                                          
024500*              VCOM INITIATOR PROGRAM NAMN                                
024600   03 IDVCINIT-14      PIC X(8).                                          
024700*              VCOM INITIATOR PROGRAM NAMN                                
024800   03 IDVCINIT-15      PIC X(8).                                          
024900*              VCOM INITIATOR PROGRAM NAMN                                
025000   03 TEFAX-1          PIC X(50).                                         
025100*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
025200   03 TEFAX-2          PIC X(50).                                         
025300*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
025400   03 TEFAX-3          PIC X(50).                                         
025500*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
025600   03 TEFAX-4          PIC X(50).                                         
025700*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
025800   03 TEFAX-5          PIC X(50).                                         
025900*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
026000   03 KVDAGAR-RESEND   PIC S9(3) COMP-3.                                  
026100*              SPARA ANT. DAGAR FÖR OMSÄNDNING                            
026200   03 IDMAIL-SENDER    PIC X(60).                                         
026300*              AVSÄNDANDE MAIL ID/PWD                                     
026400   03 TIREGDAT         PIC S9(7) COMP-3.                                  
026500*              REGISTRERINGSDATUM (ÅÅMMDD)                                
026600   03 TIUPPDAT         PIC S9(7) COMP-3.                                  
026700*              UPPDATERINGSDATUM  (ÅÅMMDD)                                
026800   03 IDUSER           PIC X(8).                                          
026900*              ANVÄNDARENS SÄKERHETS ID                                   
027000   03 TIANVDAT         PIC S9(7) COMP-3.                                  
027100*              SENAST-ANVÄND DATUM    (ÅÅMMDD)                            
027200   03 FLACIF-1         PIC X(1).                                          
027300*              SKA ACIF ANVÄNDAS?                                         
027400   03 FLACIF-2         PIC X(1).                                          
027500*              SKA ACIF ANVÄNDAS?                                         
027600   03 FLACIF-3         PIC X(1).                                          
027700*              SKA ACIF ANVÄNDAS?                                         
027800   03 FLACIF-4         PIC X(1).                                          
027900*              SKA ACIF ANVÄNDAS?                                         
028000   03 FLACIF-5         PIC X(1).                                          
028100*              SKA ACIF ANVÄNDAS?                                         
028200   03 FLACIF-6         PIC X(1).                                          
028300*              SKA ACIF ANVÄNDAS?                                         
028400   03 FLACIF-7         PIC X(1).                                          
028500*              SKA ACIF ANVÄNDAS?                                         
028600   03 FLACIF-8         PIC X(1).                                          
028700*              SKA ACIF ANVÄNDAS?                                         
028800   03 FLACIF-9         PIC X(1).                                          
028900*              SKA ACIF ANVÄNDAS?                                         
029000   03 FLACIF-10        PIC X(1).                                          
029100*              SKA ACIF ANVÄNDAS?                                         
029200   03 FLACIF-11        PIC X(1).                                          
029300*              SKA ACIF ANVÄNDAS?                                         
029400   03 FLACIF-12        PIC X(1).                                          
029500*              SKA ACIF ANVÄNDAS?                                         
029600   03 FLACIF-13        PIC X(1).                                          
029700*              SKA ACIF ANVÄNDAS?                                         
029800   03 FLACIF-14        PIC X(1).                                          
029900*              SKA ACIF ANVÄNDAS?                                         
030000   03 FLACIF-15        PIC X(1).                                          
030100*              SKA ACIF ANVÄNDAS?                                         
030200   03 TENOTE.                                                             
030300*              NOTERINGSFÄLT                                              
030400     49 TENOTE-L         PIC S9(4) COMP.                                  
030500*              NOTERINGSFÄLT                                              
030600     49 TENOTE-D         PIC X(300).                                      
030700*              NOTERINGSFÄLT                                              
030800*                                                                         
030900*** END OF VILMAII-COPY LENGTH= 2374 OLD LENGTH=                          
