000100 01  4539-WDGX4539-CTX.                                                   
000200*                                 SAMLINGSKOLLI NUMMERSERIE               
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP + IDVO +  LOW-VALUE)            
000500*                                                                         
000600     03 4539-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4539-IDVO            PIC 9(2).                                    
000900*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
001000*                                 AREA OF OPERATIONS MIX CASES            
001100     03 4539-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 4539-LOW-VALUE       PIC X(22).                                   
001500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
