000100 01  W476BL3.                                                             
000200*                                 COPYTEXT TILL CASE-REPORT LDC           
000300     03 IDAFPRCD             PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 ANTAL-KOLLI          PIC Z(3)9.                                   
000600     03 VKORDBTO             PIC Z(9)9.                                   
000700     03 KG                   PIC X(2).                                    
000800     03 IDORDNR-GRP          OCCURS 13 TIMES.                             
000900*                                 COPYTEXT TILL CASE-REPORT LDC           
001000        05 IDORDNR           PIC Z(5).                                    
001100        05 FILLER            PIC X.                                       
001200     03 KDKOLLI-EMB          PIC Z(3)9.                                   
001300     03 BETELNR              PIC X(20).                                   
001400*                                 TELEFONNUMMER                           
001500*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
