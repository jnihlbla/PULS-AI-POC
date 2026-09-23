//W430J060 JOB (640W4300100W430J060,W100),'RTN W430V1',                         
//             USER=?,PASSWORD=?,                                               
//             MSGLEVEL=(1,1),                                                  
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W43060   EXEC W430P060                                                        
//*                                                                             
//W43060.W43060D3 DD DSN=W430.REOR.DAEXDAT2(+1),DISP=(,CATLG),                  
//             SPACE=(TRK,(1,1))                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W430J060                                         
