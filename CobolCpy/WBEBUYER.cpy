000100*** EDIT ALLOWED                                                          
001600*   TABLE TO GET BUYER DESCRIPTION                                        
001700*                                                                         
001800 01  BUYER-VALUES.                                                        
001900     03  FILLER PIC X(39) VALUE                                           
                    '010 EMBALLAGE                          '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '020 KEYS                               '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '030 SPECIAL ORDER                      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '040 LOCAL SUPPLIER                     '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '100 KEY BUSINESS PHASE IN              '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '101 KEY BUSINESS PRIME                 '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '102 KEY BUSINESS DECLINE               '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '110 KEY BUSINESS BULKY PHASE IN        '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '111 KEY BUSINESS BULKY PRIME           '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '112 KEY BUSINESS BULKY DECLINE         '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '200 FUNCTION CRITICAL PHASE IN         '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '201 FUNCTION CRITICAL PRIME            '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '202 FUNCTION CRITICAL DECLINE          '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '210 FUNCTION CRITICAL BULKY PHASE IN   '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '211 FUNCTION CRITICAL BULKY PRIME      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '212 FUNCTION CRITICAL BULKY DECLINE    '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '300 PHASE IN                           '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '301 PRIME                              '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '302 DECLINE                            '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '310 BULKY PHASE IN                     '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '311 BULKY PRIME                        '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '312 BULKY DECLINE                      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '400 ACCESSORIES PHASE IN               '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '401 ACCESSORIES PRIME                  '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '402 ACCESSORIES DECLINE                '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '410 ACCESSORIES BULKY PHASE IN         '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '411 ACCESSORIES BULKY PRIME            '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '412 ACCESSORIES BULKY DECLINE          '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '500 DANGEROUS GOODS PHASE IN           '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '501 DANGEROUS GOODS PRIME              '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '502 DANGEROUS GOODS DECLINE            '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '510 DANGEROUS GOODS BULKY PHASE IN     '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '511 DANGEROUS GOODS BULKY PRIME        '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '512 DANGEROUS GOODS BULKY DECLINE      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '600 BUMPERS PHASE IN                   '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '601 BUMPERS PRIME                      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '602 BUMPERS DECLINE                    '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '610 BUMPERS BULKY PHASE IN             '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '611 BUMPERS BULKY PRIME                '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '612 BUMPERS BULKY DECLINE              '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '700 ALWAYS AIR PHASE IN                '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '701 ALWAYS AIR PRIME                   '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '702 ALWAYS AIR DECLINE                 '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '710 ALWAYS AIR BULKY PHASE IN          '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '711 ALWAYS AIR BULKY PRIME             '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '712 ALWAYS AIR BULKY DECLINE           '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '800 EXTENDED REFILL PHASE IN           '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '801 EXTENDED REFILL PRIME              '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '802 EXTENDED REFILL DECLINE            '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '810 EXTENDED REFILL BULKY PHASE IN     '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '811 EXTENDED REFILL BULKY PRIME        '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '812 EXTENDED REFILL BULKY DECLINE      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '900 TOOLS PHASE IN                     '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '901 TOOLS PRIME                        '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '902 TOOLS DECLINE                      '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '910 TOOLS BULKY PHASE IN               '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '911 TOOLS BULKY PRIME                  '.                  
001900     03  FILLER PIC X(39) VALUE                                           
                    '912 TOOLS BULKY DECLINE                '.                  
013600*                                                                         
013700 01  BUYER-TAB           REDEFINES BUYER-VALUES.                          
013800     03  BUYER-TAB-RECORD OCCURS 58 TIMES                                 
013900                          ASCENDING KEY IS BUYER-ID                       
014000                          INDEXED BY BUY-IX.                              
014100       05  BUYER-ID            PIC 9(3).                                  
014100       05  FILLER              PIC X(1).                                  
014100       05  BUYER-DESCRIPTION   PIC X(35).                                 
014200*                                                                         
