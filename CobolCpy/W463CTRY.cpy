000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS FÖR ATT ÖVERSÄTTA 2-POS          
000400*                            *** LANDKOD TILL ENGELSK LANDS-              
000500*                            *** BENÄMNING.                               
000700*                            *************************************        
001000*                                                                         
003800 01  X2-LAND-VALUES.                                                      
003900************************************X2COUNTRY                             
003901     03  FILLER    PIC X(17) VALUE 'ATAUSTRIA        '.                   
003902     03  FILLER    PIC X(17) VALUE 'BEBELGIUM        '.                   
003902     03  FILLER    PIC X(17) VALUE 'CACANADA         '.                   
003903     03  FILLER    PIC X(17) VALUE 'CHSWITZERLAND    '.                   
003903     03  FILLER    PIC X(17) VALUE 'CZCZECH REPUBLIC '.                   
003910     03  FILLER    PIC X(17) VALUE 'DEGERMANY        '.                   
003920     03  FILLER    PIC X(17) VALUE 'DKDENMARK        '.                   
003921     03  FILLER    PIC X(17) VALUE 'ESSPAIN          '.                   
003930     03  FILLER    PIC X(17) VALUE 'FIFINLAND        '.                   
003940     03  FILLER    PIC X(17) VALUE 'FRFRANCE         '.                   
004110     03  FILLER    PIC X(17) VALUE 'GBUNITED KINGDOM '.                   
004120     03  FILLER    PIC X(17) VALUE 'GRGREECE         '.                   
004120     03  FILLER    PIC X(17) VALUE 'HUHUNGARY        '.                   
004130     03  FILLER    PIC X(17) VALUE 'IEIRELAND        '.                   
004140     03  FILLER    PIC X(17) VALUE 'ISISLAND         '.                   
004160     03  FILLER    PIC X(17) VALUE 'ITITALY          '.                   
004170     03  FILLER    PIC X(17) VALUE 'LULUXEMBOURG     '.                   
004190     03  FILLER    PIC X(17) VALUE 'NLNETHERLANDS    '.                   
004191     03  FILLER    PIC X(17) VALUE 'NONORWAY         '.                   
004192     03  FILLER    PIC X(17) VALUE 'PLPOLAND         '.                   
004193     03  FILLER    PIC X(17) VALUE 'PTPORTUGAL       '.                   
004194     03  FILLER    PIC X(17) VALUE 'SESWEDEN         '.                   
004194     03  FILLER    PIC X(17) VALUE 'USUSA            '.                   
004197*                                                                         
004198 01  X2-LAND-TAB          REDEFINES X2-LAND-VALUES.                       
004199     03  X2-LAND-ING      OCCURS 23 TIMES                                 
004200                          ASCENDING KEY IS X2-SOK                         
004201                          INDEXED BY X2-IX.                               
004202       05  X2-SOK              PIC X(2).                                  
004203       05  X2-COUNTRY          PIC X(15).                                 
004210*                                                                         
004300*                                                                         
004301* ÖVERSÄTTNING DISTRIKT TILL LAND FÖR FÖRE DETTA ÖSTLÄNDER                
004302* SOM GÅR VIA ÖSTERIKE-VIPS.                                              
004303*                                                                         
004310 01  AT-LAND-VALUES.                                                      
004320************************************DISTRCOUNTRY                          
004330     03  FILLER    PIC X(25) VALUE '02370SLOVENIA            '.           
004331     03  FILLER    PIC X(25) VALUE '02371CROATIA             '.           
004332     03  FILLER    PIC X(25) VALUE '02372BOSNIA & HERZEGOVINA'.           
004333     03  FILLER    PIC X(25) VALUE '02373MACEDONIA           '.           
004334     03  FILLER    PIC X(25) VALUE '02374HUNGARIY            '.           
004335     03  FILLER    PIC X(25) VALUE '02375CZECH REPUBLIC      '.           
004336     03  FILLER    PIC X(25) VALUE '02376SLOVAKIA            '.           
004340     03  FILLER    PIC X(25) VALUE '02378AUSTRIA             '.           
004430*                                                                         
004440 01  AT-LAND-TAB          REDEFINES AT-LAND-VALUES.                       
004450     03  AT-LAND-ING      OCCURS 8 TIMES                                  
004460                          ASCENDING KEY IS AT-SOK                         
004470                          INDEXED BY AT-IX.                               
004480       05  AT-SOK              PIC 9(5).                                  
004490       05  AT-COUNTRY          PIC X(20).                                 
004491*                                                                         
004500*** END COPY W463CTRY    LENGTH=523                                       
