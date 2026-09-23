//W217J210 JOB (670W2170100W217J210,W100),'RTN W217S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W217    EXEC W217P110                                                         
//W21710.W21710D1 DD *                                                          
&IDUSER.&LISTA.&SORT.                                                           
&URVAL1.                                                                        
&URVAL2.                                                                        
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=WUT.W21710(+1),                                              
//            DSOUTZIP=&&W21710,                                                
//            ZIPDISP=(NEW,PASS,DELETE),                                        
//            CONTENT=W21710.XLS                                                
//*                                                                             
//MAIL    EXEC WMAILSND,COND.MABEND=(0,LE)                                      
//TEMPOUT   DD DSN=&&W21710,DISP=(OLD,DELETE)                                   
)SEND                                                                           
TITLE  Parts Info Excel                                                         
TO     &MAIL                                                                    
ATTACH TEMPOUT  W21710.ZIP BIN                                                  
MAIL                                                                            
 PARTINFO                                                                       
 Ordered from screen 2322                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W217J210                                         
