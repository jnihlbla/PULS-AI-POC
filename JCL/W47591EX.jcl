//W47591EX JOB (650W4750100W47591EX,W100),'RTN W475D6',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND V202                                                               
/*ROUTE XEQ NJEV2                                                               
/*ROUTE PRINT NJOVC                                                             
//W47591  EXEC WEMPTST,DSIN=W475.W475D6.W47591(+0)                              
//        EXEC V335P030,COND=(0,LT,W47591.T),                                   
//            RUTIN=W475D1,MEMBER=W47591,                                       
//            INDSN='W475.W475D6.W47591(+0)',DISP='(OLD,KEEP,KEEP)',            
//**          UTDSN='W475.EXT.W47591',DEN=3,UNIT=T6                             
//**          UTDSN='W475.EXT.W47591',DEN=3,UNIT=X9                             
//            UTDSN='W475.EXT.W47591',DEN=3,UNIT=TX                             
//SOP     EXEC WSOPEND,PROCESS=W47591EX                                         
