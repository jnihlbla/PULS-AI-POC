000100*** EDIT ALLOWED                                                          
000200 01  PI75R7H1.                                                            
000300*        CURRENT BUYER INFORMATION TO PULS                                
000400*                        ANV I PGM   ?                                    
000400*                                                                         
000600     03  IDRT                   PIC   X(3)  VALUE '7H1'.                  
000500*                                    RECORD TYPE                          
000500*                                                                         
000600     03  DATA-AREA.                                                       
000500*                                                                         
000800         05 IDPITEM-GRP.                                                  
003000            07 IDPITEM          PIC   X(20).                              
003000*                                    PURCHASED ITEM NO  JUST R            
003000            07 CD-IDPITEM       PIC   X(1)  VALUE 'V'.                    
003800*                                    PURCHASED ITEM QUALIF                
000700         05 IDPORG              PIC   X(4)  VALUE 'VCAS'.                 
003800*                                    PURCHASE ORGANIZATION                
000700         05 IDHANDLR            PIC   9(4).                               
000700*                                    HANDLER  IDINK (3 DIGITS)            
000700         05 TICHANGE-YYMMDD     PIC   9(08) VALUE ZERO.                   
000700*                                    CHANGE DATE CCYYMMDD                 
000700         05 TICHANGE-HHMMSS     PIC   9(06) VALUE ZERO.                   
000700*                                    CHANGE TIME HHMMSS                   
000700         05 IDPORG-OLD          PIC   X(04) VALUE 'VCAS'.                 
000700*                                    OLD PURCHASE ORGANIZATION            
000700         05 IDHANDLR-OLD        PIC   9(4)  VALUE ZERO.                   
000700*                                    OLD HANDLER (3 DIGITS)               
000700         05 FILLER              PIC   X(16) VALUE SPACE.                  
003800*                                                                         
