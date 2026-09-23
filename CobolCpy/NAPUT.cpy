000100*** EDIT ALLOWED                                                          
000200 01  NAP-UT.                                                              
000300*        KÖPANMODAN      TILL  NAP INKÖP                                  
000400*                        ANV I PGM W11454                                 
000400*                                                                         
000500*                                                                         
000600     03  NAP-COMMON-AREA.                                                 
000700         05 NAP-IDRT            PIC   X(04) VALUE 'PULS'.                 
000700         05 NAP-MATNR           PIC   X(18).                              
000500*                               MATERIAL NUMBER  IDARTNR                  
000550         05  FILLER  REDEFINES NAP-MATNR.                                 
000560             07  NAP-ZERO       PIC   9(9).                               
000570             07  NAP-IDARTNR    PIC   9(9).                               
000700         05 NAP-MTART           PIC   X(04)  VALUE 'NLAG'.                
000700*                               MATERIAL TYPE                             
000700         05 NAP-MBRSH           PIC   X(01)  VALUE 'N'.                   
003800*                               INDUSTRIAL SECTOR                         
000700         05 NAP-MEINS           PIC   X(03).                              
000700*                               UNIT OF MEASURE    ISO                    
000700         05 NAP-GEWEI           PIC   X(03)  VALUE 'KG'.                  
000700*                               WEIGHT UNIT                               
000700         05 NAP-MAKTX           PIC   X(40).                              
000700*                               MATERIAL DESCRIPTION  GB(SE)              
000700         05 NAP-SPRAS-ISO       PIC   X(02)  VALUE 'EN'.                  
000700*                               LANGUAGE ACCORDING TO ISO 639             
000700         05 NAP-WERKS           PIC   X(05)  VALUE 'BP2TW'.               
000700*                               PLANT                                     
000700         05 NAP-DISMM           PIC   X(02)  VALUE 'ND'.                  
000700*                               MPR TYPE                                  
000700         05 NAP-MATKL           PIC   X(09).                              
000700*                               MATERIAL GROUP/FORD COMMODITY CODE        
000700         05 NAP-MFRN            PIC   X(40)  VALUE SPACE.                 
003800*                               MANUFACTURER       NAMN                   
000700         05 NAP-MFRPN           PIC   X(40)  VALUE SPACE.                 
003800*                               MANUFACTURER PART NUMBER                  
000700         05 NAP-VPRSV           PIC   X(01)  VALUE 'S'.                   
003800*                               PRICE CONTROL INDICATOR                   
000700         05 NAP-ZEINR           PIC   X(22)  VALUE SPACE.                 
003800*                               DOCUMENT NUMBER                           
000700         05 NAP-TDLINE          PIC   X(132) VALUE SPACE.                 
003800*                               TEXT LINE                                 
000700         05 NAP-SPRAS-ISO-X     PIC   X(02)  VALUE 'EN'.                  
003800*                               LANGUAGE ACCORDING TO ISO 639             
000700*                                                  SV OR NL               
000700* LÄNGD = 328                                                             
