//W217J020 JOB (640W2170100W217J020,W100),'RTN W217S5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*IDUSER(&IDUSER)                                                              
//*LISTA(&LISTA)                                                                
//*SORT(&SORT)                                                                  
//*URVAL1(&URVAL1)                                                              
//*URVAL2(&URVAL2)                                                              
//*MAIL(&MAIL)                                                                  
//* ORDERED FROM SCREEN 2422                                                    
//*                                                                             
//W217    EXEC W217P020                                                         
//*                                                                             
//W21720.W21720D1 DD *                                                          
&IDUSER.&LISTA.&SORT.                                                           
&URVAL1.                                                                        
&URVAL2.                                                                        
//*                                                                             
//MAIL  EXEC WMAILSND,COND.MABEND=(0,LE)                                        
)SEND                                                                           
TITLE  Parts Info Excel                                                         
TO     &MAIL                                                                    
ATTACH W217.W217S5.W21720(+1) W21720.xls TEXT                                   
MAIL                                                                            
 PARTINFO,                                                                      
 Ordered from screen 2422                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W217J020                                         
